#pragma once

#include "IR/IR.hpp"
#include "PassBase.hpp"
#include "PassCollection.hpp"

#include <algorithm>
#include <unordered_map>
#include <vector>

// http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.14.3249
class IDominatorAnalysis : public SSAAnalysis<IDominatorAnalysis> {
  std::unordered_map<unsigned, unsigned> idom_map;

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
  const auto &getIDominatorMap() { return idom_map; }
  void operator()(IRHost &host);
};