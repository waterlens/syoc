#include "ASM.hpp"

using namespace SyOC;
using namespace SyOC::ARMv7a;

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