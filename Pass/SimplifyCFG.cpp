#include "SimplifyCFG.hpp"
#include "IR/IR.hpp"
#include "Pass/CFGAnalysis.hpp"
#include <cassert>

namespace SyOC {

void SimplifyCFG::clearExtraJump(BasicBlock *bb) {
  for (auto iter = bb->begin(); iter != bb->end(); ++iter)
    if (iter->isControlInstruction()) {
      while (&bb->getInstruction().back() != iter.base())
        bb->getInstruction().pop_back();
      break;
    }
}

void SimplifyCFG::straighten(Function *f) {
  if (f->refExternal())
    return;
  bool changed = true;
  while (changed) {
    changed = false;
    for (auto iter = f->block.begin(); iter != f->block.end(); ++iter) {
      if (iter == f->block.begin())
        continue;
      // has only one successor
      if (!iter->getSuccessorHead().reach_end() &&
          iter->getSuccessorHead()->next() == nullptr)
        if (iter->getSuccessorHead()->to->getPredecessor().size() == 1) {
          auto *pred = iter->getSuccessorHead()->from;
          auto *succ = iter->getSuccessorHead()->to;
          fuseBasicBlock(pred, succ);
          changed = true;
        }
    }
  }
}

void SimplifyCFG::fuseBasicBlock(BasicBlock *bb1, BasicBlock *bb2) {
  bb1->getInstruction().pop_back();
  for (auto iter = bb2->begin(); iter != bb2->end();) {
    iter->refParent() = bb1;
    auto *current = iter.base(); // must preserve this
    iter.release_and_increase(false);
    bb1->getInstruction().push_back(current);
  }
  bb2->removePredecessor(bb1);
  assert(bb2->getPredecessor().empty());
  CFGAnalysis::updateBlockPredecessor(*bb1);
  bb2->release(true);
}

void SimplifyCFG::removeDanglingBB(Function *f) {
  if (f->refExternal())
    return;
  bool changed = true;
  while (changed) {
    changed = false;
    for (auto iter = f->block.begin(); iter != f->block.end();) {
      if (iter->getPredecessor().empty() && !iter->isEntryBlock()) {
        changed = true;
        iter.release_and_increase(true);
      } else
        ++iter;
    }
  }
}

void SimplifyCFG::operator()(IRHost &host) {
  for (auto *func : host.getModule()->func)
    for (auto &bb : func->block) clearExtraJump(&bb);
  CFGAnalysis{}(host);
  for (auto *func : host.getModule()->func) removeDanglingBB(func);
  for (auto *func : host.getModule()->func) straighten(func);
}
} // namespace SyOC