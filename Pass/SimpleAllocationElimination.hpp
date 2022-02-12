#pragma once

#include "PassBase.hpp"

class SimpleAllocationElimination
  : public SSATransformation<SimpleAllocationElimination> {
private:
  void eliminateSingleDefinitionAllocation(IRHost &host);
  void eliminateLocalAllocation(IRHost &host);

public:
  SimpleAllocationElimination() = default;
  [[nodiscard]] static std::string_view getName() {
    return "Simple Allocation Elimination";
  }
  void operator()(IRHost &host);
};