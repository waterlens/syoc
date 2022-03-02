#pragma once

#include "IDominatorAnalysis.hpp"
#include "IR/IR.hpp"

#include <string_view>
#include <unordered_map>
#include <unordered_set>

namespace SyOC {
class IteratedDominanceFrontierAnalysis {
private:
  std::unordered_multimap<BasicBlock *, BasicBlock *> dominance_frontier;
  void computeDFLocal(const IDominatorAnalysis &ida, BasicBlock *x);
  void computeDFUp(const IDominatorAnalysis &ida, BasicBlock *x, BasicBlock *z);
  void computeDominanceFrontier(const IDominatorAnalysis &ida, BasicBlock *x);

public:
  IteratedDominanceFrontierAnalysis() = default;
  [[nodiscard]] static std::string_view getName() {
    return "Iterated Dominance Frontier Analysis";
  }
};
} // namespace SyOC