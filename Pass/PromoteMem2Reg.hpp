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
  std::unordered_map<Instruction *, std::vector<Instruction *>> allocaPhiMap;
  std::unordered_map<Instruction *, Instruction *> phiAllocaMap;
  std::unordered_map<BasicBlock *, std::unordered_map<Instruction *, Value *>>
    allocaLatest;
  IDominatorAnalysis idom;

  static bool isPromotable(Instruction &alloca);
  bool containValue(BasicBlock *bb, Instruction *alloc);
  Value *getValue(BasicBlock *bb, Instruction *alloc);
  void insertPhiForAlloca(IteratedDominanceFrontierAnalysis &idfa,
                          Instruction &alloca);
  void renamePhiInSuccessorBasicBlock(BasicBlock &bb, BasicBlock &pred);
  void renamePhi(const IDominatorAnalysis &ida, BasicBlock &bb);
  void promoteMem2Reg(const IDominatorAnalysis &ida,
                      IteratedDominanceFrontierAnalysis &idfa, Function &func);

public:
  PromoteMem2Reg() = default;
  [[nodiscard]] static std::string_view getName() {
    return "Promote Memory to Register";
  }
  void operator()(IRHost &host);
};
} // namespace SyOC