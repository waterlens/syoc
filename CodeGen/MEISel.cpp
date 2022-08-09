#include "CodeGen/MEISel.hpp"
#include <fmt/core.h>
#include <fmt/format.h>
#include <fmt/os.h>
using namespace SyOC;
using namespace SyOC::ARMv7a;


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
    if (I->type.pointer > 0) return Register::Type::Int;
    assert(!I->type.isVoid());
    if (I->type.isInt()) return Register::Type::Int;
    if (I->type.isFloat()) return Register::Type::Float;
  }
  return Register::Type::Int;
}

Register MEISel::CreateVirtualRegister(Value *V, int RegHint) {
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
  int new_vreg_id = (RegHint == -1) ? function->vregs_id++ : RegHint;
  Register new_reg {new_vreg_id, getRegType(V)};
  function->value_map.insert(
    std::make_pair(V, new_reg));
  return new_reg;
}

Register MEISel::CreateImmLoad(uint32_t Imm, int RegHint, Register::Type type) {
  assert(RegHint != -1);
  Register Rd {RegHint, type};
  if (test_Imm8(Imm)) {
    machine->RdOperand2(Opcode::MOV, Rd, Shift::GetImm(Imm));
  }
  else if (test_Imm8(~Imm)) {
    machine->RdOperand2(Opcode::MVN, Rd, Shift::GetImm(~Imm));
  }
  else if (test_Imm16(Imm)) {
    machine->RdImm(Opcode::MOVW, Rd, Imm & 0xffffU);
  }
  else {
    // machine->RdImm(Opcode::LDR_PC, Rd, Imm);
    machine->RdImm(Opcode::MOVW, Rd, Imm & 0xffffU);
    machine->RdImm(Opcode::MOVT, Rd, Imm >> 16);
  }
  return Rd;
}

Register MEISel::RegisterOrImm(Value *V, int RegHint) {
  // We expect Value V is a certain operand or instruction result.
  assert(V);

  if (auto *const_int = V->as<ConstantInteger *>()) {
    // mov or ldr
    // Pseudo asm ldr, =0xdeadbeef
    // Or generate movw/movt pair to load long imm.
    // ldr with [PC + offset] has only around 4KB search space
    // assembler auto-generated literal pool may out of range.
    Register Rd = CreateVirtualRegister(V, RegHint);
    return CreateImmLoad(const_int->value, Rd.id);
  }
  if (auto *globv = V->as<GlobalVariable *>()) {
    Register Rd = CreateVirtualRegister(nullptr);
    machine->RdImm(Opcode::MOVW, Rd, globv);
    machine->RdImm(Opcode::MOVT, Rd, globv);
    return Rd;
  }
  if (auto *arg = V->as<Argument *>()) {
    // We've created frame object in operator().
    Register Rd = CreateVirtualRegister(V, RegHint);
    int frame_fi = function->GetFrameObject(arg);
    machine->RdRnImm(Opcode::LDR, Rd, frame_fi, 0);
    return Rd;
  }
  if (auto *alloca = V->as<Instruction *>()) {
    if (alloca->op == OP_Allocate) {
      Register Rd = CreateVirtualRegister(V, RegHint);
      int frame_fi = function->GetFrameObject(alloca);
      machine->FrameAddr(Opcode::FRAME, Rd, frame_fi, Register{-1}, 0);
      return Rd;
    }
  }
  return function->value_map.at(V);
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
      machine->RdRnOperand2(opcode, Rd, Rn, Shift::GetImm(Imm));
      return true;
    }
  }
  Register Operand2 = RegisterOrImm(I->getOperand(1));
  machine->RdRnOperand2(opcode, Rd, Rn, Shift::GetDefaultShift(Operand2));
  return true;
}

// mul, sdiv
bool MEISel::selectRdRnRm(Instruction *I) {
  Register Rd = CreateVirtualRegister(I);
  Register Rn = RegisterOrImm(I->getOperand(0));
  Register Rm = RegisterOrImm(I->getOperand(1));
  Opcode opcode = getMachineOpcode(I->op, I->type);
  machine->RdRnRm(opcode, Rd, Rn, Rm);
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


bool MEISel::selectComparison(Instruction *I) {
  // compare
  Register Op1 = RegisterOrImm(I->getOperand(0));
  Register Op2 = RegisterOrImm(I->getOperand(1));
  machine->RnOperand2(Opcode::CMP, Op1, Shift::GetDefaultShift(Op2));
  // move
  Register Rd = CreateVirtualRegister(I);
  Condition pos = getMachineCondition(I->op);
  Condition neg = conjugateCondition(I->op);
  machine->RdOperand2(Opcode::MOV, Rd, Shift::GetImm(1), pos);
  machine->RdOperand2(Opcode::MOV, Rd, Shift::GetImm(0), neg);
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
        machine->RdRm(Opcode::CPY, Rd, SrcReg);
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
bool MEISel::selectOP_Mul(Instruction *I) { return selectRdRnRm(I); }
bool MEISel::selectOP_Div(Instruction *I) { return selectRdRnRm(I); }

// RdRnOperand2
bool MEISel::selectOP_Add(Instruction *I) { return selectRdRnOperand2(I); }
bool MEISel::selectOP_Sub(Instruction *I) { return selectRdRnOperand2(I); }
bool MEISel::selectOP_Land(Instruction *I) { return selectRdRnOperand2(I); }
bool MEISel::selectOP_Lor(Instruction *I)  { return selectRdRnOperand2(I); }
bool MEISel::selectOP_Lnot(Instruction *I) {
  Register Rd = CreateVirtualRegister(I);
  Register SrcReg = RegisterOrImm(I->getOperand(0));
  machine->RdRnOperand2(Opcode::RSB, Rd, SrcReg, Shift::GetImm(0));
  return true;
}
bool MEISel::selectOP_Neg(Instruction *I) {
  // rsb rd, src, #0: rd = #0 - src
  Register SrcReg = RegisterOrImm(I->getOperand(0));
  Register Rd = CreateVirtualRegister(I, SrcReg.id);
  machine->RdRnOperand2(Opcode::RSB, Rd, SrcReg, Shift::GetImm(0));
  return true;
}

// special expand cmp & mov
bool MEISel::selectOP_Lt(Instruction *I) { return selectComparison(I); }
bool MEISel::selectOP_Gt(Instruction *I) { return selectComparison(I); }
bool MEISel::selectOP_Le(Instruction *I) { return selectComparison(I); }
bool MEISel::selectOP_Ge(Instruction *I) { return selectComparison(I); }
bool MEISel::selectOP_Eq(Instruction *I) { return selectComparison(I); }
bool MEISel::selectOP_Ne(Instruction *I) { return selectComparison(I); }
bool MEISel::selectMemoryAccess(Instruction *I) {
  Value *Address = I->getOperand(0);
  Opcode opcode = getMachineOpcode(I->op, getAddressType(Address));
  Register Rd;
  if (I->op == OP_Store) {
    Rd = RegisterOrImm(I->getOperand(1)); // store addr, value
  } else {
    Rd = CreateVirtualRegister(I); // value = load addr
  }

  if (function->isFrameObject(Address)) {
    int stack_fi = function->GetFrameObject(Address);
    machine->RdRnImm(opcode, Rd, stack_fi, 0);
  } else {
    Register AddrReg = RegisterOrImm(Address);
    machine->RdRnImm(opcode, Rd, AddrReg, 0);
  }
  return true;
}
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
  machine->Label(Opcode::B, jmp_mbb);
  return true;
}

// Expanded Return
bool MEISel::selectOP_Return(Instruction *I) {
  auto *F = I->refParent()->as<BasicBlock *>()
    ->refParent()->as<Function *>();
  if (F->return_type.primitive_type != Type::PrimitiveType::Void) {
    // get return value.
    Register Rd = RegisterOrImm(I->getOperand(0), Register::r0);
    machine->RdRm(Opcode::CPY, Register{Register::r0}, Rd);
  }
  machine->Rm(Opcode::BX, Register{Register::lr});
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
      machine->RdRnOperand2(Opcode::AND,
                                 Rd, Dividend, Shift::GetImm(Modulo));
      return true;
    }
  }
  // otherwise, use sdiv + mls implementation.
  Register Modulo = RegisterOrImm(I->getOperand(1));
  // create a temp reg as quotient.
  Register Temp = CreateVirtualRegister(nullptr);
  machine->RdRnRm(Opcode::SDIV,
                       Temp, Dividend, Modulo);
  // Rd = Dividend - Temp * Modulo.
  machine->RdRmRnRa(Opcode::MLS,
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
      machine->RnOperand2(Opcode::CMP,
                               lhs, Shift::GetDefaultShift(rhs));
      Condition false_cond = conjugateCondition(Cmp->op);
      auto *false_mbb = function->GetBasicBlock(I->getOperand(2)->as<BasicBlock *>());
      machine->Label(Opcode::B, false_mbb, false_cond);
      auto *true_mbb = function->GetBasicBlock(I->getOperand(1)->as<BasicBlock *>());
      machine->Label(Opcode::B, true_mbb);
      return true;
    }
  }
  Register lhs = RegisterOrImm(Cond);
  machine->RnOperand2(Opcode::CMP, lhs, Shift::GetImm(0));
  // lowering jump
  auto *false_mbb = function->GetBasicBlock(I->getOperand(2)->as<BasicBlock *>());
  machine->Label(Opcode::B, false_mbb, Condition::CT_NE);
  auto *true_mbb = function->GetBasicBlock(I->getOperand(1)->as<BasicBlock *>());
  machine->Label(Opcode::B, true_mbb);
  return true;
}

/// @attention: although we may need push argument into
/// stack, there's no need to create frame object here.
/// We can just use direct [sp, #imm]
bool MEISel::selectOP_Call(Instruction *I) {
  // follow calling conventions.
  auto *Callee = I->getOperand(0)->as<Function *>();
  // SysY compiles sylib in thumb code.
  Opcode opcode = isRuntimeFunction(Callee->name) ? Opcode::BLX : Opcode::BL;
  // more than 4 args, spill to stack;
  // and protect volatile registers.
  Register Sp {Register::sp, Register::Type::Int};
  for (size_t i = Callee->arg.size(); i-- > 4;) {
    Register SrcReg = RegisterOrImm(I->getOperand(i + 1));
    int32_t Offset = (i - 4) * 4;
    machine->RdRnImm(Opcode::STR, SrcReg, Sp, Offset);
  }
  // Pass argument by r0~r3
  for (size_t i = std::min((size_t)4, Callee->arg.size());
       i-- > 0;) {
    Value *Arg = I->getOperand(i + 1);
    Register SrcReg = RegisterOrImm(Arg);
    // Copy args to r0~r3
    machine->RdRm(Opcode::CPY, Register{(int)i}, SrcReg);
    machine->Other(Opcode::CLEARUSE);
  }
  // Update stack size needed for push argument.
  MFunction *mfunc = machine->root->GetMFunction(Callee);
  if (Callee->arg.size() > 4) {
    mfunc->call_stack_args =
      std::max(mfunc->call_stack_args, Callee->arg.size() - 4);
  }
  machine->Label(opcode, mfunc);
  // Copy the return value to a virtual register.
  if (!I->type.isVoid()) {
    Register Rd = CreateVirtualRegister(I);
    machine->RdRm(Opcode::CPY, Rd, Register{Register::r0});
  }
  // There's no need to recover volatile registers
  return true;
}

bool MEISel::selectOP_Offset(Instruction *I) {
  size_t Size = I->getNumOperands();
  Value *Ptr = I->getOperand(0);
  Value *Width = I->getOperand(Size - 1);
  // Assistant assertions.
  assert(Width->is<ConstantInteger>());
  uint32_t width = Width->as<ConstantInteger *>()->value;
//  assert(test_Imm_Pow2(width));
//  int32_t log2_width = 0;
//  while (!(width & (1 << log2_width))) log2_width++;
  // Ptr can be argument, global value, alloca

  uint32_t TotalImmOffset = 0;
  uint64_t TotalSize = 1;
  std::vector<uint64_t> Sizes;
  std::vector<Value *> Indexes;
  for (size_t i = 1; i * 2 < Size; ++i) {
    Sizes.push_back(I->getOperand(i * 2 - 1)->as<ConstantInteger *>()->value);
    Indexes.push_back(I->getOperand(i * 2));
  }
  for (size_t i = Sizes.size(); i-- > 0;) {
    TotalSize *= Sizes[i];
    Sizes[i] = TotalSize;
  }
  // collecting constant imm offsets.
  Register SizeTemp = CreateVirtualRegister(nullptr);
  Register IndexTemp = CreateVirtualRegister(nullptr);
  Register Rd = CreateVirtualRegister(I);
  Sizes.push_back(1);
  bool FirstIndex = false;
  for (size_t i = 0; i < Indexes.size(); ++i) {
    if (auto *ImmIndex = Indexes[i]->as<ConstantInteger *>()) {
      TotalImmOffset += ImmIndex->value * Sizes[i + 1];
    } else {
      if (!FirstIndex) {
        Rd = CreateImmLoad(0, Rd.id);
        FirstIndex = true;
      }
      IndexTemp = RegisterOrImm(Indexes[i], IndexTemp.id);
      if (Sizes[i + 1] != 1) {
        // Hint Load non-constant index into our temp register.
        SizeTemp = CreateImmLoad(Sizes[i + 1], SizeTemp.id);
        machine->RdRmRnRa(Opcode::MLA, Rd, SizeTemp, IndexTemp, Rd);
      }
      machine->RdRnOperand2(Opcode::ADD, Rd, Rd, Shift::GetDefaultShift(IndexTemp));
    }
  }
  // considering pure Imm offset and variable offsets.
  Register OffsetReg {(FirstIndex) ? Rd.id : -1, Register::Type::Int};
  if (FirstIndex) {
    Register WidthReg = RegisterOrImm(Width);
    machine->RdRnRm(Opcode::MUL, Rd, Rd, WidthReg);
  }

  if (function->isFrameObject(Ptr)) {
    int stack_fi = function->GetFrameObject(Ptr);
    machine->FrameAddr(Opcode::FRAME, Rd, stack_fi, OffsetReg, TotalImmOffset * width);
  } else {
    Register Base = RegisterOrImm(Ptr);
    machine->RdRnOperand2(Opcode::ADD, Rd, Base,
                               Shift::GetImm(TotalImmOffset * width));
  }
  // Now Rd holds the final address.
  CreateVirtualRegister(I, Rd.id);
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
  for (GlobalVariable *globv : host->root->global)
    machine->root->global.push_back(globv);
  for (Function *func : host->getModule()->func) {
      MFunction *MF = MFunction::create(func, machine->root);
      if (func->name == "__syoc_init")
        machine->syoc_init_func = MF;
      if (func->refExternal())
        continue;
      function = MF;
      // dealing with calling convention.
      // SimpleAllocationElimination guarantees that no unused local variables,
      // but argument is not.
      // r0~r3 is volatile, we consider store r0~r3 in stack object; other args will
      // store in fix object.

      // store used r0~r3 in the entry BB.
      machine->setInsertPoint(function->GetBasicBlock(func->block.begin().base()));
      for (size_t i = 0; i < 4 && i < func->arg.size(); ++i) {
        if (!func->arg[i]->hasNoEdge()) {
          int FrameIndex = MF->CreateStackObject(func->arg[i], 4);
          machine->RdRnImm(Opcode::STR, Register{(int)i, Register::Type::Int}, FrameIndex, 0);
        }
      }
      for (size_t i = 4; i < func->arg.size(); ++i)
        MF->CreateFixObject(func->arg[i], 4); // pointer, int, float has the same 4 byte size.
      // Generate other
      for (auto &BB : func->block) {
        basic_block = function->GetBasicBlock(&BB);
        machine->setInsertPoint(basic_block);
        // select instructions
        for (auto I = BB.begin(), E = BB.end(); I != E; ++I)
          assert(selectInstruction(I.base()));
      }
      /// @attention for __syoc_init style only
      /// add a jump from main to __syoc_init
      if (func->name == "main") {
        auto FirstMInst = MF->block.begin()->insn.begin();
        machine->clearInsertPoint();
        auto *call_syoc_init = machine->Label(Opcode::BL, machine->syoc_init_func);
        FirstMInst->insert_before(call_syoc_init);
      }
  }
  mhost = machine;
}


