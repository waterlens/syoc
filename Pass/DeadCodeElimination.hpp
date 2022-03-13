#pragma once

#include "IR/IR.hpp"

namespace SyOC {
class DeadCodeElimination final {
  static void removeDeadInstruction(IRHost &host);

public:
  DeadCodeElimination() = default;
  [[nodiscard]] static std::string_view getName() { return "Dead Code Elimination"; }
  void operator()(IRHost &host) {
    removeDeadInstruction(host);
  }
};

} // namespace SyOC