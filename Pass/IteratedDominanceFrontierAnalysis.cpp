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
    if (idom.at(y.to) != x) {
      dominance_frontier.insert(x, y.to);
      dominance_frontier_set.insert(y.to);
    }
  }
}

void IteratedDominanceFrontierAnalysis::computeDFUp(
  const IDominatorAnalysis &ida, BasicBlock *x, BasicBlock *z) {
  auto [idom, _] = ida.getIDominatorMap();
  auto range = dominance_frontier.equal_range(z);
  for (auto y_iter = range.first; y_iter != range.second; ++y_iter) {
    if (!idom.contains(y_iter->second) || !idom.contains(z))
      throw std::runtime_error("y has no dominator");
    if (idom.at(y_iter->second) != x && idom.at(z) == x /* redundancy */) {
      dominance_frontier.insert(x, y_iter->second);
      dominance_frontier_set.insert(y_iter->second);
    }
  }
}

void IteratedDominanceFrontierAnalysis::computeDominanceFrontier(
  const IDominatorAnalysis &ida, BasicBlock *x) {
  auto po = ida.dominanceTreeTraversal<true, false>(x);
  for (auto *bb : po) {
    computeDFLocal(ida, bb);
    auto idominated = ida.findAllDominated(bb);
    for (auto *z : idominated) computeDFUp(ida, bb, z);
  }
}

void IteratedDominanceFrontierAnalysis::computeDominanceFrontierSet(
  const IDominatorAnalysis &ida, const std::unordered_set<BasicBlock *> &set) {
  for (auto *bb : set) computeDominanceFrontier(ida, bb);
}

void IteratedDominanceFrontierAnalysis::computeIteratedDominanceFrontierSet(
  const IDominatorAnalysis &ida, std::unordered_set<BasicBlock *> &set) {
  dominance_frontier_set.merge(set);

  bool changed = true;
  auto dfp_size = dominance_frontier_set.size();

  while (changed) {
    changed = false;

    computeDominanceFrontierSet(ida, dominance_frontier_set);
    auto dfp_size_new = dominance_frontier_set.size();

    if (dfp_size != dfp_size_new) {
      changed = true;
      dfp_size = dfp_size_new;
    }
  }  
}

} // namespace SyOC
