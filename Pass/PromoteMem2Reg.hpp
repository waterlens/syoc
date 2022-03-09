#pragma once

#include "AllocaDefinitionHelper.hpp"
#include "AssignIdentityHelper.hpp"
#include "IR/IR.hpp"
#include "PassCollection.hpp"
#include "TraversalHelper.hpp"
#include "Tree/Tree.hpp"
#include <cassert>
#include <stdexcept>
#include <string_view>
#include <unordered_set>
#include <utility>

namespace SyOC {
class PromoteMem2Reg {
  struct PairHash {
    template <class T1, class T2>
    std::size_t operator()(const std::pair<T1, T2> &p) const {
      auto h1 = std::hash<T1>()(p.first);
      auto h2 = std::hash<T2>()(p.second);
      return h1 + 0x9e3779b9 + (h2 << 6) + (h2 >> 2);
    }
  };
  std::unordered_map<Instruction *, std::vector<Instruction *>> allocaPhiMap;
  std::unordered_map<Instruction *, Instruction *> phiAllocaMap;
  std::unordered_map<std::pair<Instruction *, BasicBlock *>, Value *, PairHash>
    allocaLatestInBasicBlock;
  IDominatorAnalysis idom;

  static bool isPromotable(Instruction &alloca) {
    return alloca.op == OP_Allocate &&
           alloca.getInput()[0].from->as<SyOC::ConstantInteger *>()->value == 4;
  }

  void insertPhiForAlloca(Instruction &alloca) {
    assert(isPromotable(alloca));
    auto def_bb = findDefinitionBlock(alloca);
    for (auto *bb : def_bb) {
      auto *phi = Instruction::create(OP_Phi, PredefinedType::Int32);
      phi->refParent() = bb;
      bb->getInstruction().push_front(phi);
      phiAllocaMap[phi] = &alloca;
      allocaPhiMap[&alloca].push_back(phi);
    }
  }

  void renamePhiInBasicBlock(BasicBlock &bb) {
    for (auto insn_iter = bb.begin(); insn_iter != bb.end();) {
      if (insn_iter->op == OP_Phi) {
        if (!phiAllocaMap.contains(insn_iter.base()))
          throw std::runtime_error("phi is not associated with an allocation");
        auto *alloc = phiAllocaMap.at(insn_iter.base());
        for (auto &edge : bb.getPredecessor()) {
          auto *pred = edge.from;
          Value *latest;
          auto alloc_bb = std::make_pair(alloc, pred);
          if (!allocaLatestInBasicBlock.contains(alloc_bb))
            latest = Undef::create();
          else
            latest = allocaLatestInBasicBlock.at(alloc_bb);
          insn_iter->addInput(pred);
          insn_iter->addInput(latest);
        }
        allocaLatestInBasicBlock[{alloc, &bb}] = insn_iter.base();

      } else if (insn_iter->op == OP_Load) {
        auto *v = insn_iter->getInput()[0].from;
        if (v->is<Instruction *>() &&
            v->as<Instruction *>()->op == OP_Allocate) {
          auto *alloc = v->as<Instruction *>();
          Value *latest;
          if (allocaLatestInBasicBlock.contains({alloc, &bb}))
            latest = allocaLatestInBasicBlock.at({alloc, &bb});
          else
            latest = Undef::create();
          insn_iter->replaceAllUsesWith(latest);
          insn_iter.release_and_increase(true);
          continue;
        }
      } else if (insn_iter->op == OP_Store) {
        auto *v = insn_iter->getInput()[0].from;
        if (v->is<Instruction *>() &&
            v->as<Instruction *>()->op == OP_Allocate) {
          auto *alloc = v->as<Instruction *>();
          auto *src = insn_iter->getInput()[1].from;
          assert(alloc != nullptr && alloc->op == OP_Allocate);
          allocaLatestInBasicBlock[{alloc, &bb}] = src;
          insn_iter.release_and_increase(true);
          continue;
        }
      }
      ++insn_iter;
    }
  }

  void promoteMem2Reg(Function &func) {
    if (func.refExternal())
      return;
    auto &alloca_bb = func.block.front();
    for (auto &alloca_insn : alloca_bb) {
      if (isPromotable(alloca_insn))
        insertPhiForAlloca(alloca_insn);
    }
    auto prefix_order = traversal<false, false>(&func);
    for (auto *bb : prefix_order) renamePhiInBasicBlock(*bb);
  }

public:
  PromoteMem2Reg() = default;
  [[nodiscard]] static std::string_view getName() {
    return "Promote Memory to Register";
  }
  void operator()(IRHost &host) {
    for (auto *func : host.getModule()->func) {
      if (func->refExternal())
        continue;
      promoteMem2Reg(*func);
    }
  }
};
} // namespace SyOC