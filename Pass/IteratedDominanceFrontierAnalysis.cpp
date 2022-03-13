#include "IteratedDominanceFrontierAnalysis.hpp"

#include <cassert>
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
      dominance_frontier[x].emplace(y.to);
    }
  }
}

void IteratedDominanceFrontierAnalysis::computeDFUp(
  const IDominatorAnalysis &ida, BasicBlock *x, BasicBlock *z) {
  auto [idom, _] = ida.getIDominatorMap();
  assert(idom.at(z) == x);
  for (auto *y : dominance_frontier[z]) {
    assert(y != nullptr);
    if (!idom.contains(y) || !idom.contains(z))
      throw std::runtime_error("y has no dominator");
    if (idom.at(y) != x) {
      dominance_frontier[x].emplace(y);
    }
  }
}

void IteratedDominanceFrontierAnalysis::computeDominanceFrontier(
  const IDominatorAnalysis &ida, BasicBlock *x) {
  auto po = ida.dominanceTreeTraversal<true, false>(x);
  for (auto *bb : po) {
    computeDFLocal(ida, bb);
    auto idominated = ida.findAllIDominated(bb);
    for (auto *z : idominated) computeDFUp(ida, bb, z);
  }
}

std::unordered_set<BasicBlock *>
IteratedDominanceFrontierAnalysis::computeDominanceFrontierSet(
  const IDominatorAnalysis &ida, const std::unordered_set<BasicBlock *> &set) {
  std::unordered_set<BasicBlock *> res;
  for (auto *bb : set) computeDominanceFrontier(ida, bb);
  for (auto *bb : set) {
    if (dominance_frontier.contains(bb))
      for (auto *df : dominance_frontier.at(bb)) res.insert(df);
  }
  return res;
}

const std::unordered_set<BasicBlock *> &
IteratedDominanceFrontierAnalysis::computeIteratedDominanceFrontierSet(
  const IDominatorAnalysis &ida, std::unordered_set<BasicBlock *> &set) {
  dominance_frontier.clear();

  dominance_frontier_set = computeDominanceFrontierSet(ida, set);

  bool changed = true;
  auto dfp_size = dominance_frontier_set.size();

  while (changed) {
    changed = false;

    for (auto *bb : set) dominance_frontier_set.insert(bb);
    dominance_frontier_set =
      computeDominanceFrontierSet(ida, dominance_frontier_set);
    auto dfp_size_new = dominance_frontier_set.size();

    if (dfp_size != dfp_size_new) {
      changed = true;
      dfp_size = dfp_size_new;
    }
  }

  return dominance_frontier_set;
}

} // namespace SyOC
