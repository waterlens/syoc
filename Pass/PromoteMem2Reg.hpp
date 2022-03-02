#pragma once

#include <string_view>

namespace SyOC {
class PromoteMem2Reg {

public:
  PromoteMem2Reg() = default;
  [[nodiscard]] static std::string_view getName() {
    return "Promote Memory to Register";
  }
};
} // namespace SyOC