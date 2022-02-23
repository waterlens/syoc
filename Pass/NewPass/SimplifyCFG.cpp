#include "SimplifyCFG.hpp"

namespace YIR {

void SimplifyCFG::clearExtraJump(BasicBlock *bb) {
  for (auto iter = bb->begin(); iter != bb->end(); ++iter)
    if (iter->isControlInstruction()) {
      while (&bb->getInstruction().back() != iter.base())
        bb->getInstruction().pop_back();
      break;
    }
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
      } else ++iter;
    }
  }
}

void SimplifyCFG::operator()(IRHost &host) {
  for (auto *func : host.getModule()->func)
    for (auto &bb : func->block) clearExtraJump(&bb);
  CFGAnalysis{}(host);
  for (auto *func : host.getModule()->func) removeDanglingBB(func);
}
}