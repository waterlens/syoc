#pragma once

#include "IR/IR.hpp"
#include "TraversalHelper.hpp"

#include <algorithm>
#include <cassert>
#include <unordered_map>
#include <utility>
#include <vector>

namespace SyOC {

// http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.14.3249
class IDominatorAnalysis final {
  std::unordered_map<BasicBlock *, BasicBlock *> immediate_dominated_map;
  std::unordered_multimap<BasicBlock *, BasicBlock *> immediate_dominating_map;

  static int intersect(const std::vector<int> &doms, int pred, int now);
  void calculateImmediateDominator(Function *f);
  static void invalidateAllOrder(Function *f);

public:
  IDominatorAnalysis() = default;
  [[nodiscard]] static std::string_view getName() {
    return "Immediate Dominator Analysis";
  }
  auto getIDominatorMap() const {
    return std::make_pair(immediate_dominated_map, immediate_dominating_map);
  }
  std::unordered_set<BasicBlock *>
  findAllDominatedSet(BasicBlock *dominator) const;
  std::vector<BasicBlock *> findAllDominated(BasicBlock *dominator) const;

  void operator()(IRHost &host);
};

} // namespace SyOC