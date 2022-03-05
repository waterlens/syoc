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

class IDFDump {
  static auto findDefBlock(Instruction &alloca) {
    assert(alloca.op == OP_Allocate);
    std::unordered_set<BasicBlock *> res;
    for (auto user_iter = alloca.getEdgeHead(); !user_iter.reach_end();
         user_iter++) {
      auto *insn = user_iter->to->as<Instruction *>();
      if (insn->isDefinitionInstruction())
        res.insert(insn->getParent()->as<BasicBlock *>());
    }
    return res;
  }

  static void dumpFunctionIDF(GraphHelper &cfg, Function &func,
                              IteratedDominanceFrontierAnalysis &idfa) {
    assert(!func.refExternal());

    cfg.addNode(-func.getIdentity(), fmt::format("Function {}", func.name));

    auto &front = func.block.front();
    for (auto &insn : front) {
      if (insn.op == OP_Allocate &&
          insn.getInput()[0].from->as<SyOC::ConstantInteger *>()->value == 4) {
        cfg.addEdge(-func.getIdentity(), insn.getIdentity(), "");
        cfg.addNode(insn.getIdentity(),
                    fmt::format("var %{}", insn.getIdentity()));
        auto def_set = findDefBlock(insn);
        for (auto *def : def_set) {
          cfg.addNode(def->getIdentity(),
                      fmt::format("L{}", def->getIdentity()));
          cfg.addEdge(insn.getIdentity(), def->getIdentity(), "defined in");
        }
        auto idf_set = idfa.getIDFSet(def_set);
        for (auto *idf : idf_set) {
          cfg.addNode(idf->getIdentity(),
                      fmt::format("L{}", idf->getIdentity()));
          cfg.addEdge(insn.getIdentity(), idf->getIdentity(), "dom frontier");
        }
      }
    }
  }

public:
  IDFDump() = default;
  [[nodiscard]] static std::string_view getName() { return "IDF Dump"; }
  void operator()(IRHost &host) {
    GraphHelper g;
    IteratedDominanceFrontierAnalysis idfa;

    assignIdentity(host);
    idfa(host);
    for (auto *func : host.getModule()->func)
      if (!func->refExternal())
        dumpFunctionIDF(g, *func, idfa);

    static int idf_count = 0;
    g.outputToFile(fmt::format("dump.idf.{}.dot", idf_count++), "IDF");
  }
};

} // namespace SyOC