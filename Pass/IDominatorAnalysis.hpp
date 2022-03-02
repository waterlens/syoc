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

  template <bool Postfix>
  inline void domTreeDFS(BasicBlock *bb, std::vector<BasicBlock *> &out) const {
    if constexpr (!Postfix)
      out.push_back(bb);
    auto range = immediate_dominating_map.equal_range(bb);
    for (auto iter = range.first; iter != range.second; ++iter) {
      domTreeDFS<Postfix>(iter->second, out);
    }
    if constexpr (Postfix)
      out.push_back(bb);
  }

public:
  IDominatorAnalysis() = default;
  [[nodiscard]] static std::string_view getName() {
    return "Immediate Dominator Analysis";
  }
  std::pair<const std::unordered_map<BasicBlock *, BasicBlock *> &,
            const std::unordered_multimap<BasicBlock *, BasicBlock *> &>
  getIDominatorMap() const {
    return {immediate_dominated_map, immediate_dominating_map};
  }
  std::unordered_set<BasicBlock *>
  findAllDominatedSet(BasicBlock *dominator) const;
  std::vector<BasicBlock *> findAllDominated(BasicBlock *dominator) const;
  void operator()(IRHost &host);

  template <bool Postfix, bool Reverse>
  std::vector<BasicBlock *> dominanceTreeTraversal(BasicBlock *b) const {
    std::vector<BasicBlock *> ret;
    dfs<Postfix>(b, ret);
    if constexpr (Reverse)
      std::reverse(ret.begin(), ret.end());
    return ret;
  }
};

} // namespace SyOC