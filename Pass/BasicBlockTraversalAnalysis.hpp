#pragma once

#include "IR/IR.hpp"
#include "PassBase.hpp"
#include "Util/TrivialValueVector.hpp"

#include <unordered_map>
#include <vector>

class BasicBlockTraversalAnalysis
  : public SSAAnalysis<BasicBlockTraversalAnalysis> {

  TrivialValueVector<SSAValueHandle, 32> prefix;
  TrivialValueVector<SSAValueHandle, 32> postfix;
  IRHost *p_host;

  void dfs(BasicBlock *bb);

public:
  BasicBlockTraversalAnalysis() = default;
  [[nodiscard]] static std::string_view getName() {
    return "BasicBlock Traversal Analysis";
  }
  void clearVisitedFlag(Function *f);
  void runOnFunction(Function *f);
  [[nodiscard]] auto &getPrefix() { return prefix; }
  [[nodiscard]] auto &getPostfix() { return postfix; }
  void operator()(IRHost &host);
};