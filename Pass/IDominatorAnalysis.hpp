#pragma once

#include "IR/IR.hpp"
#include "Pass/BasicBlockTraversalAnalysis.hpp"
#include "PassBase.hpp"
#include <algorithm>
#include <unordered_map>
#include <vector>

// http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.14.3249
class IDominatorAnalysis : public SSAAnalysis<IDominatorAnalysis> {
  BasicBlockTraversalAnalysis bb_traversal;
  std::unordered_map<unsigned, unsigned> idom_map;

  int intersect(auto &doms, int pred, int now) {
    int m = pred;
    int n = now;
    while (m != n) {
      while (m < n) m = doms[m];
      while (n < m) n = doms[n];
    }
    return n;
  }

  void calculateImmediateDominator(IRHost &host, Function *f) {
    bb_traversal.runOnFunction(f);

    auto rpo = bb_traversal.getPostfix();
    bool changed = true;

    std::reverse(rpo.begin(), rpo.end());
    int id = rpo.size();
    // extra_id is assigned by the postorder traversal
    for (auto &&handle : rpo) host[handle].as<BasicBlock *>()->extra_id = --id;
    // doms[i] = idom(i), doms[i] and i is the extra_id, not the index of rpo!
    std::vector<int> doms(rpo.size(), -1);
    // doms[start_node] = start_node
    doms[rpo.size() - 1] = rpo.size() - 1;

    while (changed) {
      changed = false;
      // skip the start node, in the reverse post order
      for (int i = 1; i < rpo.size(); ++i) {
        auto bb_handle = rpo[i];
        auto *bb = host[bb_handle].as<BasicBlock *>();
        auto cur_id = bb->extra_id;

        assert(!bb->pred.empty());
        int new_idom = host[bb->pred.front()].as<BasicBlock *>()->extra_id;
        for (int j = 1; j < bb->pred.size(); ++j) {
          auto other_pred_id = host[bb->pred[j]].as<BasicBlock *>()->extra_id;
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
    for (int i = 0; i < doms_size; ++i) {
      auto rpo_idx = doms_size - i - 1;
      idom_map[rpo[rpo_idx]] = rpo[doms_size - doms[i] - 1];
    }
    changed = true;
  }

public:
  IDominatorAnalysis() = default;
  [[nodiscard]] static std::string_view getName() {
    return "Immediate Dominator Analysis";
  }

  const auto &getIDominatorMap() { return idom_map; }

  void operator()(IRHost &host) {
    bb_traversal(host);
    for (auto &&handle : host.function_table) {
      auto *func = host[handle].as<Function *>();
      if (!func->basic_block.empty())
        calculateImmediateDominator(host, func);
    }
  }
};