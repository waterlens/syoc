#include "CodeGen/MEISel.hpp"

namespace SyOC {

using ARMv7a::FrameObject;
using ARMv7a::Register;
using ARMv7a::Shift;
using ARMv7a::MInstruction;
using ARMv7a::MBasicBlock;
using ARMv7a::MFunction;
using ARMv7a::MInstHost;
using ARMv7a::Opcode;

// Operand2 Imm8
static bool test_Imm8(int32_t Imm) {
  uint32_t uImm = static_cast<uint32_t>(Imm);
  if (uImm < 0xffu) return true;
  for (int i = 1; i < 16; ++i) {
    uint32_t ror_uImm = (uImm << (i * 2)) | (uImm >> (32 - i * 2));
    if (ror_uImm < 0xffu) return true;
  }
  return false;
}

// mov/movw/movt Imm16
static bool test_Imm16(int32_t Imm) {
  uint32_t uImm = static_cast<uint32_t>(Imm);
  if (uImm < 0xffffu) return true;
  return false;
}

static bool test_LDR_STR_Imm_Offset(int32_t Imm) {
  return Imm >= -4095 && Imm <= 4095;
}

// return Armv7-a Opcode with Integer value Inst.
static Opcode getMachineIntOpcode(OpType Op) {
  switch (Op) {
  case OP_Add: return Opcode::ADD;
  case OP_Sub: return Opcode::SUB;
  case OP_Mul: return Opcode::MUL;
  case OP_Land: return Opcode::AND;
  case OP_Lor: return Opcode::ORR;
  default: return Opcode::NOP;
  }
}

static Opcode getMachineFloatOpcode(OpType Op) {
  // Not implemented.
  return Opcode::NOP;
}

static Opcode getMachineVoidOpcode(OpType Op) {
  // Not implemented.
  return Opcode::NOP;
}

// Trivial mapping from IR OpType to machine Opcode
static Opcode getMachineOpcode(OpType Op, Type T) {
  if (T.isInt()) {
    return getMachineIntOpcode(Op);
  }
  if (T.isFloat()) {
    return getMachineFloatOpcode(Op);
  }
  if (T.isVoid()) {
    return getMachineVoidOpcode(Op);
  }
}

// mov or ldr
// Pseudo asm ldr, =0xdeadbeef
// Or generate movw/movt pair to load long imm.
Register MEISel::CreatePseudoImmLoad(Value *V) {
  Register Rd = CreateVirtualRegister(V);
  if (V->is<ConstantInteger>()) {
    int32_t Imm = V->as<ConstantInteger *>()->value;
    MInstruction::RdImm(Opcode::LDR_PC, Rd, Imm);
  }
}

// add, sub, and, eor, orr, bic
bool MEISel::selectRdRnOperand2(Instruction *I) {
  Register Rd = CreateVirtualRegister(I);
  // assume const-folding guarantee both Rm and Operand2 is not constant.
  Register Rn = RegisterOrImm(I->getOperand(0));
  Register Operand2 = RegisterOrImm(I->getOperand(1));
  Opcode opcode = getMachineOpcode(I->op, I->type);
  MInstruction::RdRnOperand2(opcode, Rd, Rn, Shift::GetDefaultShift(Operand2));
  // @TODO: Set Value Map
  return true;
}

// mul
bool MEISel::selectRdRnRm(Instruction *I) {
  Register Rd = CreateVirtualRegister(I);
  Register Rn = RegisterOrImm(I->getOperand(0));
  Register Rm = RegisterOrImm(I->getOperand(1));
  Opcode opcode = getMachineOpcode(I->op, I->type);
  MInstruction::RdRnRm(opcode, Rd, Rn, Rm);
  // @TODO: Set Value Map
  return true;
}

// ldr, str, mov, movn, movw, movt
bool MEISel::selectRdRnImm(Instruction *I) {
  assert(I->isMemoryAccessInst());
  Value *Address = I->getOperand(0);
  Opcode opcode = getMachineOpcode(I->op, I->type);
  Register Rd = CreateVirtualRegister(I);

  FrameObject *Stack = function->GetStackObject(Address);
  // is a local variable.
  if (Stack != nullptr) {
    MInstruction::RdRnImm(opcode, Rd, *Stack, 0);
  } else {
    Register Rn =  RegisterOrImm(Address);
    MInstruction::RdRnImm(opcode, Rd, Rn, 0);
  }
  return true;
}

// cmps, lt, gt, le, ge, eq, ne
//
bool MEISel::selectRdOperand2(Instruction *I) {

}

// expand special IR.
bool MEISel::expandInst(Instruction *I) {
  if (I->op = OP_Offset) {
    size_t Size = I->getNumOperands();
    Value *Ptr = I->getOperand(0);
    Value *Width = I->getOperand(Size - 1);
    // Assistant assertions.
    assert(Width->is<ConstantInteger>());
    assert(Ptr->is<Argument>() ||
      (Ptr->is<Instruction>() && Ptr->as<Instruction *>()->op == OP_Offset));

    size_t Offset = 0;
    // Initialize.
    Value *Limit0 = I->getOperand(1);
    Value *Index0 = I->getOperand(2);
    Register Rd = RegisterOrImm(Index0);

    // [3, 4], [5, 6] ...
    for (int i = 2; i * 2 < Size; ++i) {
      Value *Limit = I->getOperand(i * 2 - 1);
      Value *Index = I->getOperand(i * 2);
      assert(Limit->is<ConstantInteger>());
      MInstruction::RdRnRm(Opcode::MUL, Rd, Rd,
                           RegisterOrImm(Limit));
      if (Index->is<ConstantInteger>()) {
          MInstruction::RdRnOperand2(Opcode::ADD, Rd, Rd,
                                     Shift::GetImm(Index->as<ConstantInteger *>()->value));
      } else {
        Register Rm = function->LookUpRegister(Index);
        assert(!Rm.isInvalid());
        MInstruction::RdRnOperand2(Opcode::ADD, Rd, Rd,
                                   Shift::GetDefaultShift(Rm));
      }
    }
    MInstruction::RdRnRm(Opcode::MUL, Rd, Rd, RegisterOrImm(Width));
    // Now Rd holds the total offset.
  }

  // follow calling conventions.
  if (I->op == OP_Call) {
    Function *Callee = I->getOperand(0)->as<Function *>();
    // Pass argument by r0~r3
    int RegId = Register::r0;
    for (size_t i = 1; i < std::max((size_t)4, I->getNumOperands()); ++i) {
      Value *Arg = I->getOperand(i);
      Register tmp = RegisterOrImm(Arg);
      // Copy args to r0~r3
      MInstruction::RdRm(Opcode::CPY, Register{RegId}, tmp);
      RegId++;
    }
    // more than 4 args, spill to stack.
    if (Callee->arg.size() > 4) {
      // @TODO
    }
    // Copy the return value to a virtual register.
    if (!I->type.isVoid()) {
      Register Rd = CreateVirtualRegister(I);
      MInstruction::RdRm(Opcode::CPY, Rd,Register{Register::r0});
    }
    // more than 4 args, recover the spilled args in the stack.
    if (Callee->arg.size() > 4) {
      // @TODO
    }
  }
}

bool MEISel::copyPhiNodesRegs(BasicBlock *SyOCBB) {
  for (auto I = SyOCBB->begin(), E = SyOCBB->end();
       I != E; ++I) {
    if (I->op == OP_Phi) {
      Register Rd = CreateVirtualRegister(&*I);
      // for each basicblock phi node implicates, insert COPYREG
      for (size_t i = 0; i * 2 < I->getNumOperands(); ++i) {
        // @TODO
        BasicBlock *Pred = I->getOperand(i * 2)->as<BasicBlock *>();
        Register SrcReg = RegisterOrImm(I->getOperand(i * 2 + 1));
        MInstruction::RdRm(Opcode::CPY, Rd, SrcReg);
      }
    }
  }
}

bool MEISel::selectInstruction(Instruction *I) {

}

} // end namespace SyOC