#include "ASM.hpp"

using namespace SyOC;
using namespace SyOC::ARMv7a;

MBasicBlock *MInstruction::mbb = nullptr;

void MInstruction::setInsertPoint(MBasicBlock *parent_mbb) {
  mbb = parent_mbb;
}

MInstruction *MInstruction::create() {
  auto *inst = new MInstruction;
  mbb->insn.push_back(inst);
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
  return F;
}

int MFunction::CreateStackObject(Value *V, size_t size, bool isSpill) {
  assert(V->is<Instruction>());
  auto *alloca = V->as<Instruction *>();
  assert(alloca->op == OP_Allocate);
  int index = (int)objects.size() - num_fix_object - 1;
  objects.emplace_back(0, size, alloca, isSpill);
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

int MFunction::GetFrameObject(Value *V) {
  return frame_info.at(V);
}

FrameObject *MFunction::GetFrameByIndex(int index) {
  return &objects[num_fix_object + index];
}