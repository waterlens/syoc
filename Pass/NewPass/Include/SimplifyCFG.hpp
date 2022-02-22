#pragma once

#include "IR/YIR.hpp"

#include <unordered_map>

namespace YIR {
class SimplifyCFG final {
  static void clearExtraJump(BasicBlock *bb) {
    for (auto iter = bb->begin(); iter != bb->end(); ++iter)
      if (iter->isControlInstruction()) {
        while (&bb->insn.back() != iter.base()) bb->insn.pop_back();
        break;
      }
  }
  static void removeDanglingBB(IRHost &host);

public:
  SimplifyCFG() = default;
  [[nodiscard]] static std::string_view getName() { return "Simplify CFG"; }
  void operator()(IRHost &host) {
    for (auto *func : host.getModule()->func)
      for (auto &bb : func->block) {
        clearExtraJump(&bb);
      }
  }
};

} // namespace YIR