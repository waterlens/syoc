#include "IDominatorAnalysis.hpp"

namespace SyOC {

int IDominatorAnalysis::intersect(const std::vector<int> &doms, int pred,
                                  int now) {
  int m = pred;
  int n = now;
  while (m != n) {
    while (m < n) m = doms[m];
    while (n < m) n = doms[n];
  }
  return n;
}

void IDominatorAnalysis::calculateImmediateDominator(Function *f) {
  invalidateAllOrder(f);
  auto rpo = traversal<true, true>(f);
  bool changed = true;
  auto id = rpo.size();
  // order is assigned by the reverse postorder traversal
  for (auto *bb : rpo) bb->refOrder() = --id;
  // doms[i] = idom(i), doms[i] and i is the extra_id, not the index of rpo!
  std::vector<int> doms(rpo.size(), -1);
  // doms[start_node] = start_node
  doms[rpo.size() - 1] = rpo.size() - 1;

  while (changed) {
    changed = false;
    // skip the start node, in the reverse post order
    for (int i = 1; i < rpo.size(); ++i) {
      auto *bb = rpo[i];
      auto cur_id = bb->refOrder();

      assert(cur_id != -1);
      assert(!bb->getPredecessor().empty());
      int new_idom = bb->getPredecessor().front().from->refOrder();
      assert(new_idom != -1);
      for (auto iter = ++bb->getPredecessor().cbegin();
           iter != bb->getPredecessor().cend(); ++iter) {
        auto other_pred_id = iter->from->refOrder();
        assert(other_pred_id != -1);
        if (doms[other_pred_id] != -1)
          new_idom = intersect(doms, other_pred_id, new_idom);
      }
      if (doms[cur_id] != new_idom) {
        doms[cur_id] = new_idom;
        changed = true;
      }
    }
  }
  auto doms_size = doms.size();
  // up bound is not doms_size, because last one is pointed to itself
  for (int i = 0; i < doms_size - 1; ++i) {
    auto rpo_idx = doms_size - i - 1;
    immediate_dominated_map[rpo[rpo_idx]] = rpo[doms_size - doms[i] - 1];
    immediate_dominating_map.emplace(rpo[doms_size - doms[i] - 1],
                                     rpo[rpo_idx]);
  }
}

void IDominatorAnalysis::invalidateAllOrder(Function *f) {
  assert(f != nullptr);
  for (auto &bb : f->block) bb.refOrder() = -1;
}

std::unordered_set<BasicBlock *>
IDominatorAnalysis::findAllDominatedSet(BasicBlock *dominator) const {
  std::vector<BasicBlock *> tmp;
  std::unordered_set<BasicBlock *> dominated;
  tmp.push_back(dominator);
  while (!tmp.empty()) {
    auto *bb = tmp.back();
    tmp.pop_back();
    auto range = immediate_dominating_map.equal_range(bb);
    for (auto iter = range.first; iter != range.second; ++iter) {
      dominated.insert({iter->second});
      tmp.push_back({iter->second});
    }
  }
  dominated.insert(dominator); // always dominates itself
  return dominated;
}

std::vector<BasicBlock *>
IDominatorAnalysis::findAllDominated(BasicBlock *dominator) const {
  std::vector<BasicBlock *> tmp;
  std::vector<BasicBlock *> dominated;
  tmp.push_back(dominator);
  while (!tmp.empty()) {
    auto *bb = tmp.back();
    tmp.pop_back();
    auto range = immediate_dominating_map.equal_range(bb);
    for (auto iter = range.first; iter != range.second; ++iter) {
      dominated.push_back({iter->second});
      tmp.push_back({iter->second});
    }
  }
  dominated.push_back(dominator); // always dominates itself
  return dominated;
}

std::unordered_set<BasicBlock *>
IDominatorAnalysis::findAllIDominatedSet(BasicBlock *dominator) const {
  std::unordered_set<BasicBlock *> dominated;
  auto range = immediate_dominating_map.equal_range(dominator);
  for (auto iter = range.first; iter != range.second; ++iter)
    dominated.insert(iter->second);
  return dominated;
}

std::vector<BasicBlock *>
IDominatorAnalysis::findAllIDominated(BasicBlock *dominator) const {
  std::vector<BasicBlock *> dominated;
  auto range = immediate_dominating_map.equal_range(dominator);
  for (auto iter = range.first; iter != range.second; ++iter)
    dominated.push_back(iter->second);
  return dominated;
}

void IDominatorAnalysis::operator()(IRHost &host) {
  for (auto *func : host.getModule()->func) {
    if (func->refExternal() || func->block.empty())
      continue;
    calculateImmediateDominator(func);
  }
}
} // namespace SyOC