#pragma once

#include "IR/IR.hpp"
#include "Pass/BBPredSuccAnalysis.hpp"
#include "PassBase.hpp"
#include "Util/TrivialValueVector.hpp"

#include <unordered_map>
#include <vector>

class BasicBlockTraversalAnalysis
  : public SSAAnalysis<BasicBlockTraversalAnalysis> {

  TrivialValueVector<SSAValueHandle, 32> prefix;
  TrivialValueVector<SSAValueHandle, 32> postfix;
  IRHost *p_host;

  void dfs(BasicBlock *bb) {
    auto &host = *p_host;
    bb->visited = true;
    prefix.push_back(bb->identity);
    for (auto &&succ_handle : bb->succ) {
      auto *succ = host[succ_handle].as<BasicBlock *>();
      if (!succ->visited)
        dfs(succ);
    }
    postfix.push_back(bb->identity);
  }

public:
  BasicBlockTraversalAnalysis() = default;
  [[nodiscard]] static std::string_view getName() {
    return "BasicBlock Traversal Analysis";
  }

  void clearVisitedFlag(Function *f) {
    if (f == nullptr || f->basic_block.empty())
      return;
    for (auto &&bb_handle : f->basic_block) {
      (*p_host)[bb_handle].as<BasicBlock *>()->visited = false;
    }
  }

  void runOnFunction(Function *f) {
    if (f == nullptr || f->basic_block.empty())
      return;
    clearVisitedFlag(f);
    auto &host = *p_host;
    prefix.clear();
    postfix.clear();
    auto *entry = host[f->basic_block.front()].as<BasicBlock *>();
    dfs(entry);
  }

  [[nodiscard]] auto &getPrefix() { return prefix; }
  [[nodiscard]] auto &getPostfix() { return postfix; }

  void operator()(IRHost &host) {
    BBPredSuccAnalysis{}(host);
    p_host = &host;
  }
};