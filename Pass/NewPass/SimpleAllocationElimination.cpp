#include "SimpleAllocationElimination.hpp"
#include "IDominatorAnalysis.hpp"
#include "IR/YIR.hpp"
#include "Tree/Tree.hpp"
#include "Util/Filter.hpp"
#include "Util/TrivialValueVector.hpp"
#include <cassert>

namespace YIR {
void SimpleAllocationElimination::eliminateDeadAllocation(IRHost &host) {
  for (auto *func : host.getModule()->func) {
    if (func->refExternal())
      continue;
    auto &alloca_bb = func->block.front();
    for (auto iter = alloca_bb.begin(); iter != alloca_bb.end();) {
      if (iter->op == OP_Allocate && iter->hasNoEdge())
        iter.release_and_increase(true);
      else
        ++iter;
    }
  }
}

void SimpleAllocationElimination::eliminateSingleDefinitionAllocation(
  IRHost &host) {
  IDominatorAnalysis idom;

  bool idom_initialized = false;
  auto lazy_idom = [&]() -> auto & {
    if (!idom_initialized) {
      idom(host);
      idom_initialized = true;
    }
    return idom;
  };

  for (auto *func : host.getModule()->func) {
    if (func->refExternal())
      continue;

    auto &alloca_bb = func->block.front();
    for (auto iter = alloca_bb.begin(); iter != alloca_bb.end();) {
      if (iter->op != OP_Allocate || iter->hasNoEdge()) {
        iter++;
        continue;
      }

      TrivialValueVector<Instruction *, 8> defs;
      bool skip = false;
      for (auto use_iter = iter->getImmutableEdges(); !use_iter.reach_end();
           ++use_iter) {
        auto *insn = use_iter->to->as<Instruction *>();
        if (insn->op == OP_Store || insn->op == OP_Memset0)
          defs.push_back(insn); // definition
        else if (insn->op == OP_Offset || insn->op == OP_Call) {
          skip = true;
          break; // do not touch those allocation used by offset
        }
      }

      if (skip || defs.size() != 1) {
        iter++;
        continue;
      }

      auto *def = defs.front();
      if (def->op != OP_Store) {
        iter++;
        continue;
      }

      auto *def_bb = def->getParent();
      auto *store_source = def->getInput()[1].from;
      auto all_dominated =
        lazy_idom().findAllDominatedSet(def_bb->as<BasicBlock *>());

      auto use_n = iter->getNumOfEdges() - 1; // except the store
      auto replace_n = 0;
      
      for (auto use_iter = iter->getImmutableEdges(); !use_iter.reach_end();) {
        bool inc = false;
        if (auto *load = use_iter->to->as<Instruction *>()) {
          if (load->op == OP_Load) {
            // the load inst occurs in a dominated block but not the
            // definition block
            if (all_dominated.contains(load->getParent()->as<BasicBlock *>()) &&
                load->getParent() != &alloca_bb) {
              // replace the load with the store source
              ++use_iter; // release the load will invalidate use_iter
              inc = true;
              load->replaceAllUsesWith(store_source);
              load->release(true);
              ++replace_n;
            }
          }
        }
        if (!inc)
          ++use_iter;
      }

      if (use_n == replace_n) {
        assert(def->getNumOfEdges() == 0);
        def->release(true);
      }

      iter++;
    }
  }
}

void SimpleAllocationElimination::eliminateLocalLoad(IRHost &host) {
  
}
} // namespace YIR