#pragma once

#include "IR/IR.hpp"
#include "TraversalHelper.hpp"
#include "Tree/Tree.hpp"

#include <algorithm>
#include <cassert>
#include <unordered_map>
#include <utility>
#include <vector>

namespace SyOC {

class FixTimeMeasurement final {

public:
  FixTimeMeasurement() = default;

  [[nodiscard]] static std::string_view getName() {
    return "Fix Time Measurement";
  }

  void operator()(IRHost &host) {
    Function *func_sysy_starttime = nullptr;
    Function *func_sysy_stoptime = nullptr;

    for (auto *func : host.getModule()->func) {
      if (func->name == "_sysy_starttime")
        func_sysy_starttime = func;
      if (func->name == "_sysy_stoptime")
        func_sysy_stoptime = func;
      
      if (func->refExternal())
        continue;

      for (auto &bb : func->block) {
        for (auto &insn : bb.getInstruction()) {
          if (insn.op == OP_Call) {
            if (auto *fn = insn.getInput()[0].from->as<Function *>()) {
              if (fn->name == "starttime") {
                assert(func_sysy_starttime);
                insn.getInput()[0].associate(func_sysy_starttime);
                insn.addInput(ConstantInteger::create(0));
              } else if (fn->name == "stoptime") {
                assert(func_sysy_stoptime);
                insn.getInput()[0].associate(func_sysy_stoptime);
                insn.addInput(ConstantInteger::create(0));
              }
            }
          }
        }
      }
    }
  }
};

} // namespace SyOC