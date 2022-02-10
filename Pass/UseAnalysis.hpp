#pragma once

#include "Pass/PassBase.hpp"

#include <cassert>
#include <stdexcept>

class UseAnalysis : public SSAAnalysis<UseAnalysis> {
public:
  UseAnalysis() = default;
  [[nodiscard]] static std::string_view getName() { return "Use Analysis"; }
  void operator()(IRHost &host);
};