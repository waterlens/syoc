#pragma once

#include "IR/IR.hpp"

namespace SyOC {

/// https://llvm.org/docs/Passes.html#instcombine-combine-redundant-instructions
class InstCombine final {
private:
  static std::vector<ListIterator<Instruction>> work_list;

  static void strengthReduction(IRHost &host);
  static void comparisonSimplify(IRHost &host);
  static void switchOperands(IRHost &host);
  static void groupBitwiseOperators(IRHost &host);
  static void deadCodeElimination(IRHost &host);
public:
  InstCombine() = default;
  [[nodiscard]] static std::string_view getName() { return "Combine Redundant Instructions";}
  void operator()(IRHost &host) {
    // @TODO
    comparisonSimplify(host);
    switchOperands(host);
    deadCodeElimination(host);
  }
};

} // end namespace SyOC