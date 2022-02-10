#pragma once

#include "IR/IR.hpp"
#include "PassBase.hpp"

#include <unordered_map>

class ValuePoolCompact {
public:
  ValuePoolCompact() = default;
  [[nodiscard]] static std::string_view getName() {
    return "SSA Value Pool Compact";
  }
  void operator()(IRHost &host);
};