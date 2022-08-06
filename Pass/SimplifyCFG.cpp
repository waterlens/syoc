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

void SimplifyCFG::removeUnreachable(Function *f, IRHost &host) {
  if (f->refExternal())
    return;
  std::vector<BasicBlock *> bb_work_list;
  std::vector<Instruction *> inst_work_list;
  for (auto &bb : f->block) {
    auto &br = bb.getInstruction().back();
    if (br.op == OP_Branch) {
      if (auto *Cond = br.getOperand(0)->as<ConstantInteger *>()) {
        BasicBlock *DeadBB = nullptr;
        BasicBlock *LiveBB = nullptr;
        if (Cond->value != 0) {
          LiveBB = br.getOperand(1)->as<BasicBlock *>();
          DeadBB = br.getOperand(2)->as<BasicBlock *>();
        } else {
          LiveBB = br.getOperand(2)->as<BasicBlock *>();
          DeadBB = br.getOperand(1)->as<BasicBlock *>();
        }
        bb_work_list.push_back(DeadBB);
        // replace conditional branch with a unconditional jump.
        auto *direct_jmp = host.createInstruction(OP_Jump, br.type,
                                                  {LiveBB}, &bb);
        inst_work_list.push_back(&br);
      }
    }
  }
  for (auto *dead_bb : bb_work_list) {
    for (auto inst_iter = dead_bb->begin();
         inst_iter != dead_bb->end(); ) {
      inst_iter->replaceAllUsesWith(nullptr);
      inst_iter.release_and_increase(true);
    }
    dead_bb->release(true);
  }
  for (auto *dead_inst : inst_work_list)
    dead_inst->release(true);
}

void SimplifyCFG::operator()(IRHost &host) {
  for (auto *func : host.getModule()->func) {
    // removeUnreachable(func, host);
    for (auto &bb : func->block) clearExtraJump(&bb);
  }
  CFGAnalysis{}(host);
  for (auto *func : host.getModule()->func) removeDanglingBB(func);
  for (auto *func : host.getModule()->func) straighten(func);
}
} // namespace SyOC