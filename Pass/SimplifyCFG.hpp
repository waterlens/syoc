#pragma once

#include "IR/IR.hpp"
#include "CFGAnalysis.hpp"

#include <algorithm>
#include <unordered_map>

namespace SyOC {
class SimplifyCFG final {
  static void clearExtraJump(BasicBlock *bb);
  static void removeDanglingBB(Function *f);
  static void fuseBasicBlock(BasicBlock *bb1, BasicBlock *bb2);
  static void straighten(Function *f);
  static void removeUnreachable(Function *f, IRHost &host);
public:
  SimplifyCFG() = default;
  [[nodiscard]] static std::string_view getName() { return "Simplify CFG"; }
  void operator()(IRHost &host);
};

} // namespace SyOC