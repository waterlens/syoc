#include "IteratedDominanceFrontierAnalysis.hpp"

#include <stdexcept>
#include <unordered_set>

namespace SyOC {
void IteratedDominanceFrontierAnalysis::computeDFLocal(
  const IDominatorAnalysis &ida, BasicBlock *x) {
  auto [idom, _] = ida.getIDominatorMap();
  for (auto &y : x->getSuccessor()) {
    if (!idom.contains(y.to))
      throw std::runtime_error("Successor has no dominator");
    if (idom.at(y.to) != x)
      dominance_frontier.insert(x, y.to);
  }
}
void IteratedDominanceFrontierAnalysis::computeDFUp(
  const IDominatorAnalysis &ida, BasicBlock *x, BasicBlock *z) {
  auto [idom, _] = ida.getIDominatorMap();
  auto range = dominance_frontier.equal_range(z);
  for (auto y_iter = range.first; y_iter != range.second; ++y_iter) {
    if (!idom.contains(y_iter->second) || !idom.contains(z))
      throw std::runtime_error("y has no dominator");
    if (idom.at(y_iter->second) != x && idom.at(z) == x /* redundancy */)
      dominance_frontier.insert(x, y_iter->second);
  }
}
void IteratedDominanceFrontierAnalysis::computeDominanceFrontier(
  const IDominatorAnalysis &ida, BasicBlock *x) {
  auto po = ida.dominanceTreeTraversal<true, false>(x);
  for (auto *bb : po) {
  }
}
} // namespace SyOC
