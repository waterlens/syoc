#pragma once

#include "IR/IR.hpp"
#include "Pass/Dump.hpp"
#include "Pass/IDominatorAnalysis.hpp"
#include "Pass/UseAnalysis.hpp"
#include "PassBase.hpp"
#include "Tree/Tree.hpp"
#include "Util/Filter.hpp"
#include "Util/TrivialValueVector.hpp"
#include <algorithm>
#include <unordered_map>
#include <vector>

class SimpleAllocationElimination
  : public SSATransformation<SimpleAllocationElimination> {
private:
  static void eliminateDeadAllocation(IRHost &host);

  static void eliminateSingleDefinitionAllocation(IRHost &host);

  static void eliminateSingleBlockAllocation(IRHost &host);

public:
  SimpleAllocationElimination() = default;
  [[nodiscard]] static std::string_view getName() {
    return "Simple Allocation Elimination";
  }
  void operator()(IRHost &host) {
    eliminateDeadAllocation(host);
    eliminateSingleDefinitionAllocation(host);
    eliminateSingleBlockAllocation(host);
    eliminateDeadAllocation(host);
  }
};