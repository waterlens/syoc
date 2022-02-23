#pragma once

#include "IR/YIR.hpp"
#include "CFGAnalysis.hpp"

#include <algorithm>
#include <unordered_map>

namespace YIR {
class SimplifyCFG final {
  static void clearExtraJump(BasicBlock *bb) {
    for (auto iter = bb->begin(); iter != bb->end(); ++iter)
      if (iter->isControlInstruction()) {
        while (&bb->getInstruction().back() != iter.base())
          bb->getInstruction().pop_back();
        break;
      }
  }
  static void removeDanglingBB(Function *f) {
    if (!f->refExternal())
      return;
    bool changed = true;
    while (changed) {
      changed = false;
      for (auto &bb : f->block) {
        if (bb.getPredecessor().empty() && !bb.isEntryBlock()) {
          changed = true;
          delete &bb;
        }
      }
    }
  }

public:
  SimplifyCFG() = default;
  [[nodiscard]] static std::string_view getName() { return "Simplify CFG"; }
  void operator()(IRHost &host) {
    for (auto *func : host.getModule()->func)
      for (auto &bb : func->block) clearExtraJump(&bb);
    CFGAnalysis{}(host);
    for (auto *func : host.getModule()->func) removeDanglingBB(func);
  }
};

} // namespace YIR