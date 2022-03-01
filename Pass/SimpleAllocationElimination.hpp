#pragma once

#include "IR/YIR.hpp"
#include <algorithm>
#include <unordered_map>
#include <vector>

namespace YIR {
class SimpleAllocationElimination {
private:
  static void eliminateDeadAllocation(IRHost &host);
  static void eliminateSingleDefinitionAllocation(IRHost &host);
  static void eliminateLocalLoad(IRHost &host);

public:
  SimpleAllocationElimination() = default;
  [[nodiscard]] static std::string_view getName() {
    return "Simple Allocation Elimination";
  }
  void operator()(IRHost &host) {
    eliminateDeadAllocation(host);
    eliminateSingleDefinitionAllocation(host);
    eliminateDeadAllocation(host);
    eliminateLocalLoad(host);
    eliminateDeadAllocation(host);
  }
};
} // namespace YIR