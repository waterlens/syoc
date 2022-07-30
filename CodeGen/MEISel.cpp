#include "CodeGen/MEISel.hpp"

namespace SyOC {

using ARMv7a::FrameObject;
using ARMv7a::Register;
using ARMv7a::Shift;
using ARMv7a::Condition;
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

static bool test_Imm_Pow2(int32_t Imm) {
  return (Imm == 0)? false : !(Imm & (Imm - 1));
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

// Map SSA IR comparison to ARM asm condition.
static Condition getMachineCondition(OpType Op) {
  switch (Op) {
  case OP_Gt: return Condition::CT_GT;
  case OP_Ge: return Condition::CT_GE;
  case OP_Lt: return Condition::CT_LT;
  case OP_Le: return Condition::CT_LE;
  case OP_Eq: return Condition::CT_EQ;
  case OP_Ne: return Condition::CT_NE;
  default: return Condition::CT_Any;
  }
}

// Return the negation of the comparison predicates.
static Condition conjugateCondition(OpType Op) {
  switch (Op) {
  case OP_Ge: return Condition::CT_LT;
  case OP_Gt: return Condition::CT_LE;
  case OP_Le: return Condition::CT_GT;
  case OP_Lt: return Condition::CT_GE;
  case OP_Eq: return Condition::CT_NE;
  case OP_Ne: return Condition::CT_EQ;
  default: return Condition::CT_Any;
  }
}

// mov or ldr
// Pseudo asm ldr, =0xdeadbeef
// Or generate movw/movt pair to load long imm.
// ldr with [PC + offset] has only around 4KB search space
// assembler auto-generated literal pool may out of range.
Register MEISel::CreatePseudoImmLoad(Value *V) {
  Register Rd = CreateVirtualRegister(V);
  int32_t Imm = V->as<ConstantInteger *>()->value;
  if (test_Imm8(Imm)) {
    MInstruction::RdOperand2(Opcode::MOV, Rd, Shift::GetImm(Imm));
  } else {
    MInstruction::RdImm(Opcode::LDR_PC, Rd, Imm);
  }
  return Rd;
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

// expand special IR.
bool MEISel::expandInst(Instruction *I) {
  if (I->op == OP_Mod) {
    Register Rd = CreateVirtualRegister(I);
    Register Dividend = RegisterOrImm(I->getOperand(0));
    if (I->getOperand(1)->is<ConstantInteger>()) {
      int32_t Modulo = I->getOperand(1)->as<ConstantInteger *>()->value;
      // if the module is a power of 2, use 'and' implementation.
      if (test_Imm_Pow2(Modulo)) {
        MInstruction::RdRnOperand2(Opcode::AND,
                                   Rd, Dividend, Shift::GetImm(Modulo));
        return true;
      }
    }
    // otherwise, use sdiv + mls implementation.
    Register Modulo = RegisterOrImm(I->getOperand(1));
    Register Temp = CreateVirtualRegister(nullptr);
    MInstruction::RdRnRm(Opcode::SDIV,
                         Temp, Dividend, Modulo);
    MInstruction::RdRmRnRa(Opcode::MLS,
                           Rd, Dividend, Temp, Modulo);
    return true;
  }

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

    // [Limit, Index] : [3, 4], [5, 6] ...
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
        Register Rm = RegisterOrImm(Index);
        assert(!Rm.isInvalid());
        MInstruction::RdRnOperand2(Opcode::ADD, Rd, Rd,
                                   Shift::GetDefaultShift(Rm));
      }
    }
    MInstruction::RdRnRm(Opcode::MUL, Rd, Rd, RegisterOrImm(Width));
    // Now Rd holds the total offset.
    return true;
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
    // more than 4 args, spill to stack;
    // and protect volatile registers.
    if (Callee->arg.size() > 4) {
      // @TODO
    }
    // Copy the return value to a virtual register.
    if (!I->type.isVoid()) {
      Register Rd = CreateVirtualRegister(I);
      MInstruction::RdRm(Opcode::CPY, Rd, Register{Register::r0});
    }
    // more than 4 args, recover the spilled args in the stack.
    // and recover volatile registers.
    if (Callee->arg.size() > 4) {
      // @TODO
    }
    return true;
  }

  // conditional branch affects CSPR register
  // br may use -a, !a, comparisons as branch condition.
  // InstCombine pass guarantees there would not
  if (I->op == OP_Branch) {
    // We expect after unreachable block elimination,
    // condition is not a constant int.
    // It must be argument, global variable etc.
    Value *Condition = I->getOperand(0);
    if (Condition->is<Instruction>()) {
      auto Cmp = Condition->as<Instruction *>();
      if (Cmp->isCompareInst()) {
          Register lhs = RegisterOrImm(Cmp->getOperand(0));
          Register rhs = RegisterOrImm(Cmp->getOperand(1));
          MInstruction::RdOperand2(Opcode::CMP,
                                   lhs, Shift::GetDefaultShift(rhs));
          /// @TODO: branch label
          return true;
      }
    }
    Register lhs = RegisterOrImm(Condition);
    MInstruction::RdOperand2(Opcode::CMP, lhs, Shift::GetImm(0));
    return true;
  }

  if (I->op == OP_Return) {
    // get return value.
    Register Rd = CreateVirtualRegister(I->getOperand(0));
    MInstruction::RdRm(Opcode::CPY, Register{Register::r0}, Rd);
    MInstruction::Rm(Opcode::BX, Register{Register::lr});
  }

  if (I->isCompareInst()) {
    Register Rd = CreateVirtualRegister(I);
    Condition pos = getMachineCondition(I->op);
    Condition neg = conjugateCondition(I->op);
    MInstruction::RdOperand2(Opcode::MOV, Rd, Shift::GetImm(1), pos);
    MInstruction::RdOperand2(Opcode::MOV, Rd, Shift::GetImm(0), pos);
  }
  return false;
}


/// @Attenion: Use copy reg to eliminate PHI node may
/// requires split critical edges first.
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

Register MEISel::CreateVirtualRegister(Value *V) {
  // cannot map the IR Value into a register,
  // need produce extra machine instructions.
  if (V == nullptr) {
    return Register{function->vregs_id++};
  }
  auto map_iter = function->value_map.find(V);
  if (map_iter != function->value_map.end()) {
    return Register{map_iter->second};
  }
  function->value_map.insert({V, function->vregs_id});
  return Register{function->vregs_id++};
}

Register MEISel::RegisterOrImm(Value *V) {
  // We expect Value V is a certain operand or instruction result.
  assert(V);
  if (V->is<ConstantInteger>()) {
    return CreatePseudoImmLoad(V);
  }
  return Register{function->value_map.at(V)};
}



} // end namespace SyOC