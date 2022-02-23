#pragma once

#include "IR/YIR.hpp"
#include "CFGAnalysis.hpp"

#include <algorithm>
#include <unordered_map>

namespace YIR {
class SimplifyCFG final {
  static void clearExtraJump(BasicBlock *bb);
  static void removeDanglingBB(Function *f);

public:
  SimplifyCFG() = default;
  [[nodiscard]] static std::string_view getName() { return "Simplify CFG"; }
  void operator()(IRHost &host);
};

} // namespace YIR