#pragma once

#include "IR/YIR.hpp"
#include "Tree/Tree.hpp"

#include "AssignIdentity.hpp"

#include <cassert>
#include <stdexcept>

namespace YIR {

class CFGAnalysis final {
public:
  CFGAnalysis() = default;
  [[nodiscard]] static std::string_view getName() { return "CFG Analysis"; }
  void operator()(IRHost &host);
};

} // namespace YIR