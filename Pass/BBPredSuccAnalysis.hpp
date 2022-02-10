#pragma once

#include "Pass/PassBase.hpp"

#include <cassert>
#include <stdexcept>

class BBPredSuccAnalysis final : public SSAAnalysis<BBPredSuccAnalysis> {
public:
  BBPredSuccAnalysis() = default;
  [[nodiscard]] static std::string_view getName() {
    return "Basic Block Predecessor and Successor Analysis";
  }
  void operator()(IRHost &host);
};