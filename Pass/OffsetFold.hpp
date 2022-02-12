#pragma once

#include "PassBase.hpp"

class OffsetFold
  : public SSATransformation<OffsetFold> {
public:
  OffsetFold() = default;
  [[nodiscard]] static std::string_view getName() {
    return "Simple Allocation Elimination";
  }
  void operator()(IRHost &host);
};