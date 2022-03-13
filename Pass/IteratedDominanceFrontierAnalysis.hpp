#pragma once

#include "IDominatorAnalysis.hpp"
#include "IR/IR.hpp"

#include <string_view>
#include <unordered_map>
#include <unordered_set>
#include <utility>

namespace SyOC {
class IteratedDominanceFrontierAnalysis {
public:
  using BBMap =
    std::unordered_map<BasicBlock *, std::unordered_set<BasicBlock *>>;
  using BBSet = std::unordered_set<BasicBlock *>;
  using ReturnTy = std::pair<BBMap, BBSet>;

private:
  IRHost *host;
  IDominatorAnalysis idom;
  BBMap dominance_frontier;
  BBSet dominance_frontier_set;
  void computeDFLocal(const IDominatorAnalysis &ida, BasicBlock *x);
  void computeDFUp(const IDominatorAnalysis &ida, BasicBlock *x, BasicBlock *z);
  void computeDominanceFrontier(const IDominatorAnalysis &ida, BasicBlock *x);
  std::unordered_set<BasicBlock *>
  computeDominanceFrontierSet(const IDominatorAnalysis &ida,
                              const std::unordered_set<BasicBlock *> &set);
  const std::unordered_set<BasicBlock *> &
  computeIteratedDominanceFrontierSet(const IDominatorAnalysis &ida,
                                      std::unordered_set<BasicBlock *> &set);

public:
  IteratedDominanceFrontierAnalysis() = default;
  [[nodiscard]] static std::string_view getName() {
    return "Iterated Dominance Frontier Analysis";
  }

  void operator()(IRHost &host) {
    this->host = &host;
    idom(host);
  }

  ReturnTy getIDFSet(const std::vector<BasicBlock *> &defs) {
    std::unordered_set<BasicBlock *> set;
    set.reserve(defs.size());
    for (auto *bb : defs) set.insert(bb);
    computeIteratedDominanceFrontierSet(idom, set);
    return {dominance_frontier, dominance_frontier_set};
  }

  ReturnTy getIDFSet(const std::unordered_set<BasicBlock *> &defs) {
    auto set = defs;
    computeIteratedDominanceFrontierSet(idom, set);
    return {dominance_frontier, dominance_frontier_set};
  }
};
} // namespace SyOC