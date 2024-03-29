#pragma once

#include "AssignIdentityHelper.hpp"
#include "CFGAnalysis.hpp"
#include "IDominatorAnalysis.hpp"
#include "IR/IR.hpp"
#include "Pass/IteratedDominanceFrontierAnalysis.hpp"
#include "Tree/Tree.hpp"
#include "Util/GraphHelper.hpp"
#include "Util/List.hpp"
#include "Util/StringUtil.hpp"

#include <algorithm>
#include <cassert>
#include <cstddef>
#include <fmt/core.h>
#include <fmt/format.h>
#include <fmt/os.h>
#include <stdexcept>
#include <string>
#include <string_view>
#include <unordered_set>

namespace SyOC {
class CFGDump {
  static void dumpBasicBlock(GraphHelper &cfg, BasicBlock *bb);

public:
  CFGDump() = default;
  [[nodiscard]] static std::string_view getName() { return "CFG Dump"; }
  void operator()(IRHost &host);
};

class IDominatorDump {
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
  void dumpPhiInput(Instruction *insn);
  void dumpInstruction(Instruction *insn);
  void dumpBasicBlock(BasicBlock *bb);
  void dumpFunction(Function *func);
  void dumpIRText(IRHost &host);

public:
  IRDump() = default;
  [[nodiscard]] static std::string_view getName() { return "IR Dump"; }
  void operator()(IRHost &host) {
#ifndef NDEBUG
    buffer.clear();
    buffer.reserve(32 * 1024ULL);
    dumpIRText(host);
#endif
  }
};

class IDFDump {
  static void dumpFunctionIDF(GraphHelper &cfg, Function &func,
                              IteratedDominanceFrontierAnalysis &idfa);
public:
  IDFDump() = default;
  [[nodiscard]] static std::string_view getName() { return "IDF Dump"; }
  void operator()(IRHost &host);
};

} // namespace SyOC