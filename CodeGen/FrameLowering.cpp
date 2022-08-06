
#include "FrameLowering.hpp"

using namespace SyOC;
using namespace SyOC::ARMv7a;

void FrameLowering::lowering(MFunction *mfunc, MInstHost *host) {
  size_t num_frames = mfunc->objects.size();
  size_t num_callee_saved = mfunc->callee_saved.size();
  int32_t total_stack_offset, total_fix_offset;

  // callee saved register.
  total_stack_offset = num_callee_saved * 4;
  // stack local variables.
  for (size_t i = mfunc->num_fix_object; i < num_frames; ++i) {
    total_stack_offset += mfunc->objects[i].Size;
    mfunc->objects[i].Offset = -total_stack_offset;
  }
  // fixed object, arguments.
  total_fix_offset = 0;
  for (size_t i = mfunc->num_fix_object; i-- > 0;) {
    mfunc->objects[i].Offset = total_fix_offset;
    total_fix_offset += mfunc->objects[i].Size;
  }


  // lowering frame info to stack
  for (auto &BB : mfunc->block) {
    for (auto I = BB.insn.begin(), E = BB.insn.end();
         I != E; ++I)
    {
      // lowering address calculation.
      if (I->op == Opcode::FRAME) {
        Register Rd = I->ra;
        Register RegOffset = I->rb;
        Register Sp {Register::sp, Register::Type::Int};
        int FrameIndex = std::get<int>(I->rc.base);
        int32_t Imm = std::get<int32_t>(I->rc.offset_or_else);
        Imm += mfunc->objects[FrameIndex + mfunc->num_fix_object].Offset
               + total_stack_offset;
        if (Imm == 0 && RegOffset.isInvalid()) {
          auto *minst = host->RdRm(Opcode::CPY, Rd, Sp);
          I->insert_before(minst);
        }
        if (Imm != 0) {
          auto *minst = host->RdRnOperand2(Opcode::ADD, Rd,
                                           Sp, Shift::GetImm(Imm));
          I->insert_before(minst);
        }
        if (!RegOffset.isInvalid()) {
          auto *minst = host->RdRnOperand2(Opcode::ADD, Rd,
                                           Sp, Shift::GetDefaultShift(RegOffset));
          I->insert_before(minst);
        }
        work_list.push_back(&*I);
      }
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
        work_list.push_back(&*I);
      }
    }
  }
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
  if (list != 0) {
    auto *push = host->Reglist(Opcode::PUSH, RegisterList{list});

    FirstMInst->insert_before(push);
  }
  if (mfunc->num_fix_object != 0) {
    auto *get_fp = host->RdRnOperand2(
      Opcode::ADD, Fp, Sp, Shift::GetImm(mfunc->callee_saved.size() * 4));
    FirstMInst->insert_before(get_fp);
  }
}

// pop callee saved register and add sp.
void FrameLowering::emitEpilogue(MFunction *mfunc, MInstHost *host) {
  RegisterList::RegVector list = 0;
  for (auto saved_info : mfunc->callee_saved) {
    if (!saved_info.SpilledToReg)
      list |= (1U << saved_info.Reg.id);
  }
  auto LastMInst = mfunc->block.back().insn.back();
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
    lowering(mfunc, &host);
    emitPrologue(mfunc, &host);
    emitEpilogue(mfunc, &host);
  }
  deadCodeElimination();
}