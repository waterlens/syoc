#pragma once

#include "IR/IR.hpp"
#include "Tree/Tree.hpp"

#include "AssignIdentityHelper.hpp"

#include <cassert>
#include <stdexcept>

namespace SyOC {

class CFGAnalysis final {
public:
  CFGAnalysis() = default;
  [[nodiscard]] static std::string_view getName() { return "CFG Analysis"; }
  void operator()(IRHost &host);
};

} // namespace SyOC