#pragma once

#include "IR/IR.hpp"

namespace SyOC {

/// https://llvm.org/docs/Passes.html#instcombine-combine-redundant-instructions
class InstCombine final {
private:
  std::vector<ListIterator<Instruction>> work_list;

  void strengthReduction(IRHost &host);
  void comparisonSimplify(IRHost &host);
  void switchOperands(IRHost &host);
  void groupBitwiseOperators(IRHost &host);
  void deadCodeElimination(IRHost &host);
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