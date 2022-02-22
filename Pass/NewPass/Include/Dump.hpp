#pragma once

#include "IR/YIR.hpp"
#include "Tree/Tree.hpp"
#include "Util/GraphHelper.hpp"
#include "Util/List.hpp"
#include "Util/StringUtil.hpp"
#include "fmt/core.h"

#include <algorithm>
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
  size_t id;
  static std::string dumpType(const Type &ty);
  static std::string dumpUser(Value *value);
  static std::string
  dumpFunctionParameterList(const std::vector<Argument *> &param);
  static std::string dumpInstructionInput(Value *value);
  void dumpAllGlobalVariable(IRHost &host);
  void dumpInstruction(Instruction *insn);
  void dumpBasicBlock(BasicBlock *bb);
  void dumpFunction(Function *func);
  void assignIdentity(IRHost &host);
  void dumpIRText(IRHost &host);

public:
  IRDump() = default;
  [[nodiscard]] static std::string_view getName() { return "IR Dump"; }
  void operator()(IRHost &host) {
    buffer.clear();
    buffer.reserve(32 * 1024ULL);
    dumpIRText(host);
  }
};
} // namespace YIR