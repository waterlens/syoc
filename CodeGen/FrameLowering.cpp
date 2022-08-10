
#include "FrameLowering.hpp"

using namespace SyOC;
using namespace SyOC::ARMv7a;


void FrameLowering::lowering(MFunction *mfunc, MInstHost *host) {
  size_t num_frames = mfunc->objects.size();
  size_t num_callee_saved = mfunc->callee_saved.size();
  int32_t total_stack_offset, total_fix_offset;

  total_stack_offset = total_fix_offset = 0;

  // callee saved register.
  total_stack_offset = num_callee_saved * 4;
  // stack local variables.
  for (size_t i = mfunc->num_fix_object; i < num_frames; ++i) {
    total_stack_offset += mfunc->objects[i].Size;
    mfunc->objects[i].Offset = -total_stack_offset;
  }

  // save call stack args
  total_stack_offset += mfunc->call_stack_args * 4;

  // fixed object, arguments.
  total_fix_offset = 0;
  for (size_t i = mfunc->num_fix_object; i-- > 0;) {
    mfunc->objects[i].Offset = total_fix_offset;
    total_fix_offset += mfunc->objects[i].Size;
  }
#ifndef NDEBUG
  fmt::print("Function: {}, Stack Size: {}\n", mfunc->name, total_stack_offset);
#endif
  // GNUEABI requires Stack Pointer aligned at 8 byte;
  // round up stack size aligned at 8 byte.
  total_stack_offset = (total_stack_offset + stack_align - 1) & ~(stack_align - 1);

  // lowering frame info to stack
  for (auto &BB : mfunc->block) {
    for (auto I = BB.insn.begin(), E = BB.insn.end();
         I != E; ++I)
    {
      // lowering address calculation.
      if (I->op == Opcode::FRAME) {
        Register Rd = I->ra;
        Register RegOffset = I->rb;
        // calculate final offsets.
        int FrameIndex = std::get<int>(I->rc.base);
        int32_t BaseImm = std::get<int32_t>(I->rc.offset_or_else);
        int32_t SpImm = BaseImm +
          mfunc->objects[FrameIndex + mfunc->num_fix_object].Offset +
          total_stack_offset;
        int32_t FpImm = BaseImm +
           mfunc->objects[FrameIndex + mfunc->num_fix_object].Offset;
        // choose the nearest sp/fp as the addressing base reg.
        Register StackBase {(abs(SpImm) > abs(FpImm)) ? Register::fp : Register::sp,
                          Register::Type::Int};
        int32_t Imm = (abs(SpImm) > abs(FpImm)) ? FpImm : SpImm;


        // insert new addressing offsets.
        // pure frame
        if (Imm == 0 && RegOffset.isInvalid()) {
          auto *minst = host->RdRm(Opcode::CPY, Rd, StackBase);
          I->insert_before(minst);
        }
        // frame + reg + offset
        if (Imm != 0 && !RegOffset.isInvalid()) {
          /// @attention
          /// frame	%11:gpr, #fi:2 offset %11:gpr, 0
          /// may produce wrong code:
          /// 	add 	r5, sp, #4
          ///	add 	r5, r5, r5
          auto *minst = host->RdRm(Opcode::CPY, Rd,
                                           RegOffset);
          I->insert_before(minst);
          minst = host->RdRnOperand2(Opcode::ADD, Rd,
                                     StackBase, Shift::GetImm(Imm));
          I->insert_before(minst);
        } else {
          // frame + imm
          if (Imm != 0) {
            auto *minst = host->RdRnOperand2(Opcode::ADD, Rd, StackBase,
                                             Shift::GetImm(Imm));
            I->insert_before(minst);
          }
          // frame + reg
          if (!RegOffset.isInvalid()) {
            auto *minst = host->RdRnOperand2(Opcode::ADD, Rd, StackBase,
                                             Shift::GetDefaultShift(RegOffset));
            I->insert_before(minst);
          }
        }
        work_list.push_back(I.base());
      }

      // ad-hoc addressing with STR, LDR with a frame object.
      if ((I->op == Opcode::STR || I->op == Opcode::LDR) &&
          I->rc.isStack())
      {
        Register Rd = I->ra;
        int FrameIndex = std::get<int>(I->rc.base);
        int32_t Imm = std::get<int32_t>(I->rc.offset_or_else);
        Register Sp {Register::sp, Register::Type::Int};
        // Location = Stack Size + Offset
        Imm += mfunc->objects[FrameIndex + mfunc->num_fix_object].Offset
              + total_stack_offset;
        auto *minst = host->RdRnImm(I->op, Rd, Sp, Imm);
        I->insert_before(minst);
        work_list.push_back(I.base());
      }
    }
  }
  final_stack_size = total_stack_offset - num_callee_saved * 4;
}

// push callee saved register and sub sp.
void FrameLowering::emitPrologue(MFunction *mfunc, MInstHost *host) {
  RegisterList::RegVector list = 0;
  for (auto saved_info : mfunc->callee_saved) {
    if (!saved_info.SpilledToReg)
      list |= (1U << saved_info.Reg.id);
  }
  auto FirstMInst = mfunc->block.begin()->insn.begin();
  Register Fp {Register::fp, Register::Type::Int};
  Register Sp {Register::sp, Register::Type::Int};
  // push {rxx, rxx}
  if (list != 0) {
    auto *push = host->Reglist(Opcode::PUSH, RegisterList{list});
    FirstMInst->insert_before(push);
  }
  // add r11, sp, #imm
  auto *get_fp = host->RdRnOperand2(
    Opcode::ADD, Fp, Sp, Shift::GetImm(mfunc->callee_saved.size() * 4));
  FirstMInst->insert_before(get_fp);
  // sub sp, sp #imm
  if (list != 0 && final_stack_size != 0) {
    auto *reduce_sp =
      host->RdRnOperand2(Opcode::SUB, Sp, Sp, Shift::GetImm(final_stack_size));
    FirstMInst->insert_before(reduce_sp);
  }
}

// pop callee saved register and add sp.
void FrameLowering::emitEpilogue(MFunction *mfunc, MInstHost *host) {
  RegisterList::RegVector list = 0;
  for (auto saved_info : mfunc->callee_saved) {
    if (!saved_info.SpilledToReg)
      list |= (1U << saved_info.Reg.id);
  }
  auto &LastMInst = mfunc->block.back().insn.back();
  Register Sp {Register::sp, Register::Type::Int};
  Register Fp {Register::fp, Register::Type::Int};
  // add sp, sp #imm
  if (list != 0 && final_stack_size != 0) {
    auto *increase_sp =
      host->RdRnOperand2(Opcode::ADD, Sp, Sp, Shift::GetImm(final_stack_size));
    LastMInst.insert_before(increase_sp);

  }
  if (list != 0) {
    auto *pop = host->Reglist(Opcode::POP, RegisterList{list});
    LastMInst.insert_before(pop);
  }

}

void FrameLowering::deadCodeElimination() {
  for (auto *minst : work_list)
    minst->release(true);
  work_list.clear();
}

void FrameLowering::operator()(MInstHost &host) {
  host.clearInsertPoint();
  for (auto *mfunc : host.root->function) {
    if (mfunc->refExternal)
      continue;
    lowering(mfunc, &host);
    deadCodeElimination();
    emitPrologue(mfunc, &host);
    emitEpilogue(mfunc, &host);
    legalizeOffset(mfunc, &host);
#ifndef NDEBUG
    fmt::print("Function: {}, Stack Size: {}\n", mfunc->name, final_stack_size);
#endif
  }
}

void inline CreateLongImmLoad(uint32_t Imm, Register::Type type,
                              MInstruction *I, MInstHost *MHost) {
  Register Rd {Register::r12, type};
  if (test_Imm16(Imm)) {
    auto *Movw = MHost->RdImm(Opcode::MOVW, Rd, Imm & 0xffffU);
    I->insert_before(Movw);
  }
  else {
    // machine->RdImm(Opcode::LDR_PC, Rd, Imm);
    auto *MovW = MHost->RdImm(Opcode::MOVW, Rd, Imm & 0xffffU);
    I->insert_before(MovW);
    auto *MovT = MHost->RdImm(Opcode::MOVT, Rd, Imm >> 16);
    I->insert_before(MovT);
  }
}

void FrameLowering::legalizeOffset(MFunction *MF, MInstHost *MHost) {
  MHost->clearInsertPoint();
  Register Aux {Register::r12, Register::Type::Int};
  for (auto &BB : MF->block) {
    for (auto I = BB.insn.begin(), E = BB.insn.end();
         I != E; ++I)
    {
       if (I->op == Opcode::STR &&
         std::holds_alternative<int32_t>(I->rc.offset_or_else)) {
         int32_t Imm = std::get<int32_t>(I->rc.offset_or_else);
         if (!test_Imm8(Imm)) {
           CreateLongImmLoad(Imm, Register::Type::Int, I.base(), MHost);
           I->op = Opcode::STR_REG;
           I->rb = std::get<Register>(I->rc.base);
           I->rc.base = Aux;
         }
       }
       if (I->op == Opcode::LDR &&
         std::holds_alternative<int32_t>(I->rc.offset_or_else)) {
         int32_t Imm = std::get<int32_t>(I->rc.offset_or_else);
         if (!test_Imm8(Imm)) {
           CreateLongImmLoad(Imm, Register::Type::Int, I.base(), MHost);
           I->op = Opcode::LDR_REG;
           I->rb = std::get<Register>(I->rc.base);
           I->rc.base = Aux;
         }
       }
       if (I->op == Opcode::ADD || I->op == Opcode::SUB) {
         Shift SF = std::get<Shift>(I->rc.offset_or_else);
         if (!test_Imm8(SF.imm)) {
           CreateLongImmLoad(SF.imm, Register::Type::Int, I.base(), MHost);
           I->rc.offset_or_else = Shift::GetDefaultShift(Aux);
         }
       }
    }
  }
}