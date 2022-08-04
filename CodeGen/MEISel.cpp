#include "CodeGen/MEISel.hpp"
#include <fmt/core.h>
#include <fmt/format.h>
#include <fmt/os.h>
using namespace SyOC;
using namespace SyOC::ARMv7a;


// Operand2 Imm8
static bool test_Imm8(int32_t Imm) {
  auto uImm = static_cast<uint32_t>(Imm);
  if (uImm < 0xffU) return true;
  for (int i = 1; i < 16; ++i) {
    uint32_t ror_uImm = (uImm << (i * 2)) | (uImm >> (32 - i * 2));
    if (ror_uImm < 0xffU) return true;
  }
  return false;
}

// mov/movw/movt Imm16
static bool test_Imm16(int32_t Imm) {
  auto uImm = static_cast<uint32_t>(Imm);
  return uImm < 0xffffU;
}

static bool test_LDR_STR_Imm_Offset(int32_t Imm) {
  return Imm >= -4095 && Imm <= 4095;
}

static bool test_Imm_Pow2(int32_t Imm) {
  if (Imm == 0) return false;
  return (Imm & (Imm - 1)) == 0;
}

// return Armv7-a Opcode with Integer value Inst.
static Opcode getMachineIntOpcode(OpType Op) {
  switch (Op) {
  case OP_Add: return Opcode::ADD;
  case OP_Sub: return Opcode::SUB;
  case OP_Mul: return Opcode::MUL;
  case OP_Div: return Opcode::SDIV;
  case OP_Land: return Opcode::AND;
  case OP_Lor: return Opcode::ORR;
  case OP_Store: return Opcode::STR;
  case OP_Load: return Opcode::LDR;
  default:
    throw std::runtime_error("No mapped machine opcode");
  }
}

static Opcode getMachineFloatOpcode(OpType Op) {
  // Not implemented.
  return Opcode::NOP;
}

static Opcode getMachineVoidOpcode(OpType Op) {
  throw std::runtime_error("Void Instructions should not be lowered");
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


//===-- Utility Gadgets------------ ----*- C++ -*-===//
//===---------------------------------------------===//

// Return the register type corresponding to IR Value type.
static Register::Type getRegType(Value *V) {
  if (V->is<ConstantInteger>()) {
    return Register::Type::Int;
  }
  if (auto *I = V->as<Instruction *>()) {
    assert(!I->type.isVoid());
    if (I->type.isInt()) return Register::Type::Int;
    if (I->type.isFloat()) return Register::Type::Float;
  }
  return Register::Type::Int;
}

Register MEISel::CreateVirtualRegister(Value *V) {
  // cannot map the IR Value into a register,
  // need produce extra machine instructions.
  if (V == nullptr) {
    return Register{function->vregs_id++, Register::Type::Int};
  }
  auto map_iter = function->value_map.find(V);
  if (map_iter != function->value_map.end()) {
    return map_iter->second;
  }
  // create new virtual reg.
  Register new_reg {function->vregs_id++, getRegType(V)};
  function->value_map.insert(
    std::make_pair(V, new_reg));
  return new_reg;
}

Register MEISel::RegisterOrImm(Value *V) {
  // We expect Value V is a certain operand or instruction result.
  assert(V);
  if (auto *const_int = V->as<ConstantInteger *>()) {
    // mov or ldr
    // Pseudo asm ldr, =0xdeadbeef
    // Or generate movw/movt pair to load long imm.
    // ldr with [PC + offset] has only around 4KB search space
    // assembler auto-generated literal pool may out of range.
    Register Rd = CreateVirtualRegister(V);
    uint32_t Imm = V->as<ConstantInteger *>()->value;
    if (test_Imm8(Imm)) {
      auto *inst = MInstruction::RdOperand2(Opcode::MOV, Rd, Shift::GetImm(Imm));
      basic_block->insn.push_back(inst);
    } else {
      // MInstruction::RdImm(Opcode::LDR_PC, Rd, Imm);
      MInstruction::RdImm(Opcode::MOVW, Rd, Imm & 0xffffU);
      MInstruction::RdImm(Opcode::MOVT, Rd, Imm >> 16);
    }
    return Rd;
  }
  if (auto *globv = V->as<GlobalVariable *>()) {
    Register Rd = CreateVirtualRegister(nullptr);
    MInstruction::RdImm(Opcode::MOVW, Rd, globv);
    MInstruction::RdImm(Opcode::MOVT, Rd, globv);
    return Rd;
  }
  if (auto *arg = V->as<Argument *>()) {
    Register Rd = CreateVirtualRegister(V);
    int frame_idx = function->GetFrameObject(arg);
    MInstruction::RdRnImm(Opcode::LDR, Rd, frame_idx, 0);
    return Rd;
  }
  return function->value_map.at(V);
}

// As compiled runtime library is compiled by thumb
// We should use blx to make appropriate function call.
static bool isRuntimeFunction(std::string_view name) {
  return (name == "getint" || name == "getch" || name == "getfloat" ||
      name == "getarray" || name == "getfarray" ||
      name == "putint" || name == "putch" || name == "putfloat" ||
      name == "putarray" || name == "putfarray" ||
      name == "starttime" || name == "stoptime");
}

//===-- Instruction Selection Phase ----*- C++ -*-===//
//===---------------------------------------------===//

// add, sub, and, eor, orr, bic
bool MEISel::selectRdRnOperand2(Instruction *I) {
  Register Rd = CreateVirtualRegister(I);
  Opcode opcode = getMachineOpcode(I->op, I->type);
  // assume const-folding guarantee both Rm and Operand2 is not constant.
  Register Rn = RegisterOrImm(I->getOperand(0));
  // Optimization: Check if the Operand2 fit imm8
  if (auto *Op2 = I->getOperand(1)->as<ConstantInteger *>()) {
    int32_t Imm = static_cast<int32_t>(Op2->value & 0xFFFFFFFFFU);
    if (test_Imm8(Imm)) {
      MInstruction::RdRnOperand2(opcode, Rd, Rn, Shift::GetImm(Imm));
      return true;
    }
  }
  Register Operand2 = RegisterOrImm(I->getOperand(1));
  MInstruction::RdRnOperand2(opcode, Rd, Rn, Shift::GetDefaultShift(Operand2));
  return true;
}

// mul, sdiv
bool MEISel::selectRdRnRm(Instruction *I) {
  Register Rd = CreateVirtualRegister(I);
  Register Rn = RegisterOrImm(I->getOperand(0));
  Register Rm = RegisterOrImm(I->getOperand(1));
  Opcode opcode = getMachineOpcode(I->op, I->type);
  MInstruction::RdRnRm(opcode, Rd, Rn, Rm);
  return true;
}

// ldr, str
// @TODO: asr, lsl are RdRnImm type, but IR doesn't support currently.
bool MEISel::selectRdRnImm(Instruction *I) { return false; }

static Type getAddressType(Value *V) {
  if (auto *globv = V->as<GlobalVariable *>())
    return globv->type;
  if (auto *arg = V->as<Argument *>())
    return arg->type;
  if (auto *inst = V->as<Instruction *>())
    return inst->type;
  throw std::runtime_error("Expect address as GlobalVariable, Argument or Instruction");
}

bool MEISel::selectMemoryAccess(Instruction *I) {
  Value *Address = I->getOperand(0);
  Opcode opcode = getMachineOpcode(I->op, getAddressType(Address));
  Register Rd;
  if (I->op == OP_Store) {
    Rd = RegisterOrImm(I->getOperand(1)); // store addr, value
  } else {
    Rd = CreateVirtualRegister(I); // value = load addr
  }
/*
  FrameObject Stack = function->GetStackObject(Address);
  // is a local variable.
  if (Stack.index != -1) {
    auto *inst = MInstruction::RdRnImm(opcode, Rd, Stack, 0);
    basic_block->insn.push_back(inst);
  } else {
    Register Rn = RegisterOrImm(Address);
    auto *inst = MInstruction::RdRnImm(opcode, Rd, Rn, 0);
    basic_block->insn.push_back(inst);
  }*/
  return true;
}


bool MEISel::selectComparison(Instruction *I) {
  // compare
  Register Op1 = RegisterOrImm(I->getOperand(0));
  Register Op2 = RegisterOrImm(I->getOperand(1));
  MInstruction::RdOperand2(Opcode::CMP, Op1, Shift::GetDefaultShift(Op2));
  // move
  Register Rd = CreateVirtualRegister(I);
  Condition pos = getMachineCondition(I->op);
  Condition neg = conjugateCondition(I->op);
  MInstruction::RdOperand2(Opcode::MOV, Rd, Shift::GetImm(1), pos);
  MInstruction::RdOperand2(Opcode::MOV, Rd, Shift::GetImm(0), pos);
  return true;
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
  return true;
}


// Should not be selected.
bool MEISel::selectOP_None(Instruction *I) { return false; }
bool MEISel::selectOP_End(Instruction *I) { return false; }
bool MEISel::selectOP_Phi(Instruction *I) { return true; }

// RdRnImm
bool MEISel::selectOP_Mul(Instruction *I) { return selectRdRnImm(I); }
bool MEISel::selectOP_Div(Instruction *I) { return selectRdRnImm(I); }

// RdRnOperand2
bool MEISel::selectOP_Add(Instruction *I) { return selectRdRnOperand2(I); }
bool MEISel::selectOP_Sub(Instruction *I) { return selectRdRnOperand2(I); }
bool MEISel::selectOP_Land(Instruction *I) { return selectRdRnOperand2(I); }
bool MEISel::selectOP_Lor(Instruction *I)  { return selectRdRnOperand2(I); }
bool MEISel::selectOP_Lnot(Instruction *I) {
  return false;
}
bool MEISel::selectOP_Neg(Instruction *I) {
  // rsb rd, src, #0: rd = #0 - src
  Register SrcReg = RegisterOrImm(I->getOperand(0));
  Register Rd = CreateVirtualRegister(I);
  MInstruction::RdRnOperand2(Opcode::RSB, Rd, SrcReg, Shift::GetImm(0));
  return true;
}

// special expand cmp & mov
bool MEISel::selectOP_Lt(Instruction *I) { return selectComparison(I); }
bool MEISel::selectOP_Gt(Instruction *I) { return selectComparison(I); }
bool MEISel::selectOP_Le(Instruction *I) { return selectComparison(I); }
bool MEISel::selectOP_Ge(Instruction *I) { return selectComparison(I); }
bool MEISel::selectOP_Eq(Instruction *I) { return selectComparison(I); }
bool MEISel::selectOP_Ne(Instruction *I) { return selectComparison(I); }

// memory access
bool MEISel::selectOP_Store(Instruction *I) { return selectMemoryAccess(I); }
bool MEISel::selectOP_Load(Instruction *I)  { return selectMemoryAccess(I); }
bool MEISel::selectOP_Allocate(Instruction *I) {
  size_t size = I->getOperand(0)->as<ConstantInteger *>()->value;
  function->CreateStackObject(I, size);
  return true;
}

// Unconditional Branch
bool MEISel::selectOP_Jump(Instruction *I) {
  auto *jmp_bb = I->getOperand(0)->as<BasicBlock *>();
  MBasicBlock *jmp_mbb = function->GetBasicBlock(jmp_bb);
  MInstruction::Label(Opcode::B, jmp_mbb);
  return true;
}

// Expanded Return
bool MEISel::selectOP_Return(Instruction *I) {
  auto *F = I->refParent()->as<BasicBlock *>()
    ->refParent()->as<Function *>();
  if (F->return_type.primitive_type != Type::PrimitiveType::Void) {
    // get return value.
    Register Rd = CreateVirtualRegister(I->getOperand(0));
    MInstruction::RdRm(Opcode::CPY, Register{Register::r0}, Rd);
  }
  MInstruction::Rm(Opcode::BX, Register{Register::lr});
  return true;
}

// ARM doesn't support single instruction modulo.
bool MEISel::selectOP_Mod(Instruction *I) {
  Register Rd = CreateVirtualRegister(I);
  Register Dividend = RegisterOrImm(I->getOperand(0));
  // if the module is a power of 2 constant, use 'and' implementation.
  if (auto *Operand2 = I->getOperand(1)->as<ConstantInteger *>()) {
    int32_t Modulo = Operand2->value;
    if (test_Imm_Pow2(Modulo)) {
      MInstruction::RdRnOperand2(Opcode::AND,
                                 Rd, Dividend, Shift::GetImm(Modulo));
      return true;
    }
  }
  // otherwise, use sdiv + mls implementation.
  Register Modulo = RegisterOrImm(I->getOperand(1));
  // create a temp reg as quotient.
  Register Temp = CreateVirtualRegister(nullptr);
  MInstruction::RdRnRm(Opcode::SDIV,
                       Temp, Dividend, Modulo);
  // Rd = Dividend - Temp * Modulo.
  MInstruction::RdRmRnRa(Opcode::MLS,
                         Rd, Dividend, Temp, Modulo);
  return true;
}

// Conditional Jump
// Create a B{cond} and a B jump, breaks basicblock property.
bool MEISel::selectOP_Branch(Instruction *I) {
  // conditional branch affects CSPR register
  // br may use -a, !a, comparisons as branch condition.
  // We expect after unreachable block elimination,
  // condition is not a constant int.
  // It must be argument, global variable etc.
  Value *Cond = I->getOperand(0);
  if (Cond->is<Instruction>()) {
    // should still lower the comparison because there may
    // other comparison in between.
    auto *Cmp = Cond->as<Instruction *>();
    if (Cmp->isCompareInst()) {
      Register lhs = RegisterOrImm(Cmp->getOperand(0));
      Register rhs = RegisterOrImm(Cmp->getOperand(1));
      MInstruction::RnOperand2(Opcode::CMP,
                               lhs, Shift::GetDefaultShift(rhs));
      Condition false_cond = conjugateCondition(I->op);
      auto *false_mbb = function->GetBasicBlock(I->getOperand(2)->as<BasicBlock *>());
      MInstruction::Label(Opcode::B, false_mbb, false_cond);
      auto *true_mbb = function->GetBasicBlock(I->getOperand(1)->as<BasicBlock *>());
      MInstruction::Label(Opcode::B, true_mbb);
      return 0;
    }
  }
  Register lhs = RegisterOrImm(Cond);
  MInstruction::RnOperand2(Opcode::CMP, lhs, Shift::GetImm(0));
  // lowering jump
  auto *false_mbb = function->GetBasicBlock(I->getOperand(2)->as<BasicBlock *>());
  MInstruction::Label(Opcode::B, false_mbb, Condition::CT_NE);
  auto *true_mbb = function->GetBasicBlock(I->getOperand(1)->as<BasicBlock *>());
  MInstruction::Label(Opcode::B, true_mbb);
  return true;
}

bool MEISel::selectOP_Call(Instruction *I) {
  // follow calling conventions.
  auto *Callee = I->getOperand(0)->as<Function *>();
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

bool MEISel::selectOP_Offset(Instruction *I) {
  size_t Size = I->getNumOperands();
  Value *Ptr = I->getOperand(0);
  Value *Width = I->getOperand(Size - 1);
  // Assistant assertions.
  assert(Width->is<ConstantInteger>());
  int32_t width = Width->as<ConstantInteger *>()->value;
  assert(test_Imm_Pow2(width));
  int32_t log2_width = 0;
  while (!(width & (1 << log2_width))) log2_width++;
  // Ptr can be argument, global value,

  size_t Offset = 0;
  // Initialize.
  Value *Limit0 = I->getOperand(1);
  Value *Index0 = I->getOperand(2);
  Register Rd = RegisterOrImm(Index0);

  // [Limit, Index] : [3, 4], [5, 6] ...
  for (size_t i = 2; i * 2 < Size; ++i) {
    Value *Limit = I->getOperand(i * 2 - 1);
    Value *Index = I->getOperand(i * 2);
    assert(Limit->is<ConstantInteger>());
    MInstruction::RdRnRm(Opcode::MUL, Rd, Rd,
                         RegisterOrImm(Limit));
    if (auto *Imm = Index->as<ConstantInteger *>()) {
      if (test_Imm8(static_cast<int32_t>(Imm->value))) {
        MInstruction::RdRnOperand2(Opcode::ADD, Rd, Rd,
          Shift::GetImm(Index->as<ConstantInteger *>()->value));
        continue;
      }
    }
    Register Rm = RegisterOrImm(Index);
    assert(!Rm.isInvalid());
    MInstruction::RdRnOperand2(Opcode::ADD, Rd, Rd,
                               Shift::GetDefaultShift(Rm));
  }
  Register Base = RegisterOrImm(Ptr);
  MInstruction::RdRnOperand2(Opcode::ADD, Rd, Base,
                             Shift{Shift::Type::SF_LSL, Rd, log2_width});
  // Now Rd holds the final address.
  return true;
}

// Not implemented.
bool MEISel::selectOP_Memset0(Instruction *I) { return false; }


bool MEISel::selectInstruction(Instruction *I) {
  switch (I->op) {
#define OpcodeDefine(type, name) \
  case type: return select##type(I);
#include "Common/Common.def"
  default: return false;
  }
}



void MEISel::operator()(IRHost *host, MInstHost *&mhost) {
  machine = new MInstHost;
  // Build skeleton of MModule, MFunctions.
  // Note that MBasicBlocks would be created with
  // the same order of IR BasicBlocks
  for (Function *func : host->getModule()->func) {
      fmt::print("{}\n", func->name);
      MFunction *MF = MFunction::create(func, machine->root);
      function = MF;
      // dealing with calling convention.
      for (auto *Arg : func->arg) {
        // @TODO:
        int RegId = Register::r0;

      }
      for (size_t i = 4; i < func->arg.size(); ++i)
        MF->CreateFixObject(func->arg[i], 4); // pointer, int, float has the same 4 byte size.
      for (auto &BB : func->block) {
        basic_block = function->GetBasicBlock(&BB);
        MInstruction::setInsertPoint(basic_block);
        // select instructions
        for (auto I = BB.begin(), E = BB.end(); I != E; ++I)
          assert(selectInstruction(I.base()));
      }
  }
  mhost = machine;
}


