#pragma once

#include "AssignIdentityHelper.hpp"
#include "IDominatorAnalysis.hpp"
#include "IR/YIR.hpp"
#include "CFGAnalysis.hpp"
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

namespace YIR {
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
  void operator()(IRHost &host) {
    IDominatorAnalysis ida;
    GraphHelper idg;
    ida(host);
    assignIdentity(host);

    std::unordered_set<BasicBlock *> visited;
    const auto &idominated_map = ida.getIDominatorMap().first;
    for (auto [idominated, idominator] : idominated_map) {
      if (!visited.contains(idominated)) {
        idg.addNode(idominated->getIdentity(),
                    fmt::format("L{}", idominated->getIdentity()));
        visited.insert(idominated);
      }
      if (!visited.contains(idominator)) {
        idg.addNode(idominator->getIdentity(),
                    fmt::format("L{}", idominator->getIdentity()));
        visited.insert(idominator);
      }
      idg.addEdge(idominator->getIdentity(), idominated->getIdentity(), "");
    }
    visited.clear();

    static int g_count = 0;
    idg.outputToFile(fmt::format("dump.idom.{}.dot", g_count), "IDominator");
  }
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