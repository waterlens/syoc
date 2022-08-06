#include "ASM.hpp"

using namespace SyOC;
using namespace SyOC::ARMv7a;


MInstruction *MInstruction::MInstruction::create() {
  auto *inst = new MInstruction;
  return inst;
}

MBasicBlock *MBasicBlock::create(MFunction *F) {
  MBasicBlock *B = new MBasicBlock;
  B->m_prev = B->m_next = nullptr;
  F->block.push_back(B);
  return B;
}

MFunction *MFunction::create(Function *func, MModule *module) {
  MFunction *F = new MFunction;
  F->name = func->name;
  for (auto B = func->block.begin(), BE = func->block.end();
       B != BE; ++B) {
    auto MBB = MBasicBlock::create(F);
    F->bb_map.insert({B.base(), MBB});
  }
  module->function.push_back(F);
  module->func_map.insert(std::make_pair(func, F));
  return F;
}

int MFunction::CreateStackObject(Value *V, size_t size, bool isSpill) {
  assert(V->is<Instruction>() || V->is<Argument>());
  if (V->is<Instruction>())
    objects.emplace_back(0, size, V->as<Instruction *>(), isSpill);
  else
    objects.emplace_back(0, size, nullptr, isSpill);
  int index = (int)objects.size() - num_fix_object - 1;
  frame_info.insert(std::make_pair(V, index));
  return index;
}

int MFunction::CreateFixObject(Value *V, size_t size) {
  assert(V->is<Argument>());
  // create argument (LLVM fix object)
  auto *arg = V->as<Argument *>();
  FrameObject frame(0, size, nullptr, false);
  int index = -++num_fix_object;
  objects.insert(objects.begin(), frame);
  frame_info.insert(std::make_pair(V, index));
  return index;
}

bool MFunction::isFrameObject(Value *V) {
  return frame_info.count(V) != 0;
}

int MFunction::GetFrameObject(Value *V) {
  return frame_info.at(V);
}

FrameObject *MFunction::GetFrameByIndex(int index) {
  return &objects[num_fix_object + index];
}

MFunction *MModule::GetMFunction(Function *F) {
  return func_map.at(F);
}

#define assert_op_format(fmt) assert(get_op_format(op) == Format::fmt)
MInstruction *MInstHost::Other(Opcode op) {
  assert_op_format(IF_Other);
  auto *p = MInstruction::create();
  p->op = op;
  return p;
}


MInstruction *MInstHost::RdRnRm(Opcode op, Register rd, Register rn, Register rm,
                                Condition cond) {
  assert_op_format(IF_RdRnRm);
  auto *p = MInstruction::create();
  p->op = op;
  p->ra = rd;
  p->rb = rn;
  p->rc.base = rm;
  p->cond = cond;
  if (machine_basic_block != nullptr)
    machine_basic_block->insn.push_back(p);
  return p;
}

// p->inter or global memory access with definite register
MInstruction *MInstHost::RdRnImm(Opcode op, Register rd, Register rn, int imm,
                             Condition cond ) {
  assert_op_format(IF_RdRnImm);
  auto *p = MInstruction::create();
  p->op = op;
  p->ra = rd;
  p->rc.base = rn;
  p->rc.offset_or_else = imm;
  p->cond = cond;
  if (machine_basic_block != nullptr)
    machine_basic_block->insn.push_back(p);
  return p;
}

// stack memory access with intermediate FrameObject rep->esentation,
// will be lowered to [sp-> #offset] afterward.
MInstruction *MInstHost::RdRnImm(Opcode op, Register rd, int frame_idx, int32_t imm,
                             Condition cond) {
  assert_op_format(IF_RdRnImm);
  auto *p = MInstruction::create();
  p->op = op;
  p->ra = rd;
  p->rc.base = frame_idx;
  p->rc.offset_or_else = imm;
  p->cond = cond;
  if (machine_basic_block != nullptr)
    machine_basic_block->insn.push_back(p);
  return p;
}

// load common imm.
MInstruction *MInstHost::RdImm(Opcode op, Register rd, int32_t imm,
                           Condition cond ) {
  assert_op_format(IF_RdImm);
  auto *p = MInstruction::create();
  p->op = op;
  p->ra = rd;
  p->rc.offset_or_else = imm;
  p->cond = cond;
  if (machine_basic_block != nullptr)
    machine_basic_block->insn.push_back(p);
  return p;
}

// load global variable 32bit address.
MInstruction *MInstHost::RdImm(Opcode op, Register rd, GlobalVariable *globv,
                           Condition cond ) {
  assert_op_format(IF_RdImm);
  auto *p = MInstruction::create();
  p->op = op;
  p->ra = rd;
  p->rc.offset_or_else = globv;
  p->cond = cond;
  if (machine_basic_block != nullptr)
    machine_basic_block->insn.push_back(p);
  return p;
}

MInstruction *MInstHost::RdRm(Opcode op, Register rd, Register rm,
                          Condition cond ) {
  assert_op_format(IF_RdRm);
  auto *p = MInstruction::create();
  p->op = op;
  p->ra = rd;
  p->rb = rm;
  p->cond = cond;
  if (machine_basic_block != nullptr)
    machine_basic_block->insn.push_back(p);
  return p;
}

MInstruction *MInstHost::RdRmRnRa(Opcode op, Register rd, Register rm,
                              Register rs, Register rn,
                              Condition cond ) {
  assert_op_format(IF_RdRmRnRa);
  auto p = MInstruction::create();
  p->op = op;
  p->ra = rd;
  p->rb = rm;
  p->rc.base = rs;
  p->rc.offset_or_else = rn;
  p->cond = cond;
  if (machine_basic_block != nullptr)
    machine_basic_block->insn.push_back(p);
  return p;
}

MInstruction *MInstHost::RdLoRdHiRnRm(Opcode op, Register lo, Register hi,
                                  Register rm, Register rs,
                                  Condition cond ) {
  assert_op_format(IF_RdLoRdHiRnRm);
  auto *p= MInstruction::create();
  p->op = op;
  p->ra = lo;
  p->rb = hi;
  p->rc.base = rm;
  p->rc.offset_or_else = rs;
  p->cond = cond;
  if (machine_basic_block != nullptr)
    machine_basic_block->insn.push_back(p);
  return p;
}

MInstruction *MInstHost::RdRnOperand2(Opcode op, Register rd, Register rm,
                                  Shift sf,
                                  Condition cond ) {
  assert_op_format(IF_RdRnOperand2);
  auto *p = MInstruction::create();
  p->op = op;
  p->ra = rd;
  p->rb = rm;
  p->rc.offset_or_else = sf;
  p->cond = cond;
  if (machine_basic_block != nullptr)
    machine_basic_block->insn.push_back(p);
  return p;
}

MInstruction *MInstHost::RdOperand2(Opcode op, Register rd, Shift sf,
                                Condition cond ) {
  assert_op_format(IF_RdOperand2);
  auto *p = MInstruction::create();
  p->op = op;
  p->ra = rd;
  p->rc.offset_or_else = sf;
  p->cond = cond;
  if (machine_basic_block != nullptr)
    machine_basic_block->insn.push_back(p);
  return p;
}

MInstruction *MInstHost::RnOperand2(Opcode op, Register rn, Shift sf,
                                Condition cond ) {
  assert_op_format(IF_RnOperand2);
  auto *p = MInstruction::create();
  p->op = op;
  p->rb = rn;
  p->rc.offset_or_else = sf;
  p->cond = cond;
  if (machine_basic_block != nullptr)
    machine_basic_block->insn.push_back(p);
  return p;
}

MInstruction *MInstHost::Label(Opcode op, MBasicBlock *label,
                           Condition cond ) {
  assert_op_format(IF_Label);
  auto *p = MInstruction::create();
  p->op = op;
  p->rc.offset_or_else = label;
  p->cond = cond;
  if (machine_basic_block != nullptr)
    machine_basic_block->insn.push_back(p);
  return p;
}

MInstruction *MInstHost::Label(Opcode op, MFunction *mfunc,
                               Condition cond) {
  assert_op_format(IF_Label);
  auto *p = MInstruction::create();
  p->op = op;
  p->rc.offset_or_else = mfunc;
  p->cond = cond;
  if (machine_basic_block != nullptr)
    machine_basic_block->insn.push_back(p);
  return p;
}

MInstruction *MInstHost::Rm(Opcode op, Register rm,
                        Condition cond ) {
  assert_op_format(IF_Rm);
  auto *p = MInstruction::create();
  p->op = op;
  p->rb = rm;
  p->cond = cond;
  if (machine_basic_block != nullptr)
    machine_basic_block->insn.push_back(p);
  return p;
}

MInstruction *MInstHost::Reglist(Opcode op, RegisterList list,
                             Condition cond ) {
  assert_op_format(IF_Reglist);
  auto *p= MInstruction::create();
  p->op = op;
  p->rc.offset_or_else = list;
  p->cond = cond;
  if (machine_basic_block != nullptr)
    machine_basic_block->insn.push_back(p);
  return p;
}

MInstruction *MInstHost::FrameAddr(Opcode op, Register rd, int frame_fi,
                               Register offset_reg, int32_t offset_imm,
                               Condition cond ) {
  assert_op_format(IF_FrameAddr);
  auto *p = MInstruction::create();
  p->op = op;
  p->ra = rd;
  p->rb = offset_reg;
  p->rc.base = frame_fi;
  p->rc.offset_or_else = offset_imm;
  p->cond = cond;
  if (machine_basic_block != nullptr)
    machine_basic_block->insn.push_back(p);
  return p;
}

#undef assert_op_format