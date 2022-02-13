#pragma once

#include "IR/IR.hpp"
#include "PassBase.hpp"
#include "PassCollection.hpp"

#include <algorithm>
#include <unordered_map>
#include <vector>

// http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.14.3249
class IDominatorAnalysis : public SSAAnalysis<IDominatorAnalysis> {
  std::unordered_map<unsigned, unsigned> immediate_dominated_map;
  std::unordered_multimap<unsigned, unsigned> immediate_dominating_map;

  static int intersect(const std::vector<int> &doms, int pred, int now);
  void calculateImmediateDominator(IRHost &host,
                                   BasicBlockTraversalAnalysis &bb_traversal,
                                   Function *f);
  static void invalidateAllExtraId(IRHost &host, Function *f);

public:
  IDominatorAnalysis() = default;
  [[nodiscard]] static std::string_view getName() {
    return "Immediate Dominator Analysis";
  }
  std::pair<const std::unordered_map<unsigned, unsigned> &,
            const std::unordered_multimap<unsigned, unsigned> &>
  getIDominatorMap() const {
    return {immediate_dominated_map, immediate_dominating_map};
  }
  std::unordered_set<SSAValueHandle>
  findAllDominatedSet(SSAValueHandle dominator) const;
  std::vector<SSAValueHandle> findAllDominated(SSAValueHandle dominator) const;
  void operator()(IRHost &host);
};