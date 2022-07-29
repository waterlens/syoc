#pragma once

#include "IR/IR.hpp"

namespace SyOC {

/// https://llvm.org/docs/Passes.html#instcombine-combine-redundant-instructions
class InstCombine final {
  static void strengthReduction(IRHost &host);
  static void comparisonSimplify(IRHost &host);
  static void switchOperands(IRHost &host);
  static void groupBitwiseOperators(IRHost &host);

public:
  InstCombine() = default;
  [[nodiscard]] static std::string_view getName() { return "Combine Redundant Instructions";}
  void operator()(IRHost &host) {
    // @TODO
    comparisonSimplify(host);
  }
};

} // end namespace SyOC