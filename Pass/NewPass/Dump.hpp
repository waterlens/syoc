#pragma once

#include "IR/YIR.hpp"
#include "Tree/Tree.hpp"
#include "Util/GraphHelper.hpp"

#include <cassert>
#include <cstddef>
#include <fmt/format.h>
#include <fmt/os.h>
#include <stdexcept>
#include <string>
#include <string_view>
#include <unordered_set>

namespace YIR {
class CFGDump {
  static void dumpCFG(IRHost &host);

public:
  CFGDump() = default;
  [[nodiscard]] static std::string_view getName() { return "CFG Dump"; }
  void operator()(IRHost &host);
};

class IDominatorDump {
  static void dumpIDominator(IRHost &host);

public:
  IDominatorDump() = default;
  [[nodiscard]] static std::string_view getName() { return "IDom Dump"; }
  void operator()(IRHost &host);
};

class IRDump {
  std::string buffer;
  static std::string dumpType(const Type &ty);
  void dumpIRText(IRHost &host);

public:
  IRDump() = default;
  [[nodiscard]] static std::string_view getName() { return "IR Dump"; }
  void operator()(IRHost &host);
};
} // namespace YIR