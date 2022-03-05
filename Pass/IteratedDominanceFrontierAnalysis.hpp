#pragma once

#include "IDominatorAnalysis.hpp"
#include "IR/IR.hpp"

#include <string_view>
#include <unordered_map>
#include <unordered_set>

namespace SyOC {
class IteratedDominanceFrontierAnalysis {
private:
  IRHost *host;
  IDominatorAnalysis idom;
  std::unordered_map<BasicBlock *, std::unordered_set<BasicBlock *>>
    dominance_frontier;
  std::unordered_set<BasicBlock *> dominance_frontier_set;
  void computeDFLocal(const IDominatorAnalysis &ida, BasicBlock *x,
                      std::unordered_set<BasicBlock *> &res);
  void computeDFUp(const IDominatorAnalysis &ida, BasicBlock *x, BasicBlock *z,
                   std::unordered_set<BasicBlock *> &res);
  void computeDominanceFrontier(const IDominatorAnalysis &ida, BasicBlock *x,
                                std::unordered_set<BasicBlock *> &res);
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

  auto getIDFSet(const std::vector<BasicBlock *> &defs) {
    std::unordered_set<BasicBlock *> set;
    set.reserve(defs.size());
    for (auto *bb : defs) set.insert(bb);
    computeIteratedDominanceFrontierSet(idom, set);
    return dominance_frontier_set;
  }

  auto getIDFSet(const std::unordered_set<BasicBlock *> &defs) {
    auto set = defs;
    computeIteratedDominanceFrontierSet(idom, set);
    return dominance_frontier_set;
  }
};
} // namespace SyOC