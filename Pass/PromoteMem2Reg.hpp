#pragma once

#include "AllocaDefinitionHelper.hpp"
#include "AssignIdentityHelper.hpp"
#include "IR/IR.hpp"
#include "Pass/IDominatorAnalysis.hpp"
#include "Pass/IteratedDominanceFrontierAnalysis.hpp"
#include "PassCollection.hpp"
#include "TraversalHelper.hpp"
#include "Tree/Tree.hpp"
#include <cassert>
#include <stdexcept>
#include <string_view>
#include <unordered_map>
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
  std::unordered_map<BasicBlock *, std::unordered_map<Instruction *, Value *>>
    allocaLatest;
  IDominatorAnalysis idom;

  static bool isPromotable(Instruction &alloca) {
    return alloca.op == OP_Allocate &&
           alloca.getInput()[0].from->as<SyOC::ConstantInteger *>()->value == 4;
  }

  bool containValue(BasicBlock *bb, Instruction *alloc) {
    return allocaLatest.contains(bb) && allocaLatest.at(bb).contains(alloc);
  }

  Value *getValue(BasicBlock *bb, Instruction *alloc) {
    return allocaLatest.at(bb).at(alloc);
  }

  void insertPhiForAlloca(IteratedDominanceFrontierAnalysis &idfa,
                          Instruction &alloca) {
    assert(isPromotable(alloca));
    auto def_bb = findDefinitionBlock(alloca);
    auto [_, idf] = idfa.getIDFSet(def_bb);
    for (auto *bb : idf) {
      auto *phi = Instruction::create(OP_Phi, PredefinedType::Int32);
      phi->refParent() = bb;
      bb->getInstruction().push_front(phi);
      phiAllocaMap[phi] = &alloca;
      allocaPhiMap[&alloca].push_back(phi);
    }
  }

  void renamePhiInSuccessorBasicBlock(BasicBlock &bb, BasicBlock &pred) {
    for (auto &phi : bb.getInstruction()) {
      if (phi.op != OP_Phi)
        break;
      if (!phiAllocaMap.contains(&phi))
        throw std::runtime_error("phi is not associated with an allocation");
      auto *alloc = phiAllocaMap.at(&phi);
      if (containValue(&pred, alloc)) {
        phi.addInput(&pred);
        phi.addInput(getValue(&pred, alloc));
      }
    }
  }

  void renamePhi(const IDominatorAnalysis &ida, BasicBlock &bb) {
    for (auto insn_iter = bb.begin(); insn_iter != bb.end();) {
      if (insn_iter->op == OP_Phi) {
        if (!phiAllocaMap.contains(insn_iter.base()))
          throw std::runtime_error("phi is not associated with an allocation");
        auto *alloc = phiAllocaMap.at(insn_iter.base());
        allocaLatest[&bb][alloc] = insn_iter.base();
      } else if (insn_iter->op == OP_Load) {
        auto *v = insn_iter->getInput()[0].from;
        if (v->is<Instruction *>() &&
            v->as<Instruction *>()->op == OP_Allocate) {
          auto *alloc = v->as<Instruction *>();
          Value *latest;
          if (containValue(&bb, alloc))
            latest = getValue(&bb, alloc);
          else
            throw std::runtime_error("undef here");
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
          allocaLatest[&bb][alloc] = src;
          insn_iter.release_and_increase(true);
          continue;
        }
      }
      ++insn_iter;
    }

    auto [_, idom] = ida.getIDominatorMap();
    auto range = idom.equal_range(&bb);
    for (auto iter = range.first; iter != range.second; iter++) {
      if (allocaLatest.contains(&bb))
        allocaLatest[iter->second] = allocaLatest[&bb];
    }

    for (auto &e : bb.getSuccessor()) {
      auto *succ = e.to;
      renamePhiInSuccessorBasicBlock(*succ, bb);
    }
  }

  void promoteMem2Reg(const IDominatorAnalysis &ida,
                      IteratedDominanceFrontierAnalysis &idfa, Function &func) {
    if (func.refExternal())
      return;
    auto &alloca_bb = func.block.front();
    for (auto &alloca_insn : alloca_bb) {
      if (isPromotable(alloca_insn))
        insertPhiForAlloca(idfa, alloca_insn);
    }
    auto bfs = bfsTraversal<false>(&func);
    for (auto *bb : bfs) renamePhi(ida, *bb);
    clearVisited(&func);
  }

public:
  PromoteMem2Reg() = default;
  [[nodiscard]] static std::string_view getName() {
    return "Promote Memory to Register";
  }
  void operator()(IRHost &host) {
    IteratedDominanceFrontierAnalysis idfa;
    IDominatorAnalysis ida;
    idfa(host);
    ida(host);

    for (auto *func : host.getModule()->func) {
      if (func->refExternal())
        continue;
      promoteMem2Reg(ida, idfa, *func);
    }
  }
};
} // namespace SyOC