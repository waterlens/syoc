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

  }
}

bool MEISel::selectInstruction(Instruction *I) {

}

} // end namespace SyOC