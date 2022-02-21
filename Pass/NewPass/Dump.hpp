#pragma once

#include "IR/YIR.hpp"
#include "Tree/Tree.hpp"
#include "Util/GraphHelper.hpp"
#include "Util/StringUtil.hpp"
#include "Util/TrivialValueList.hpp"
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
  static std::string dumpType(const Type &ty) {
    std::string buffer;
    switch (ty.primitive_type) {
    case Type::PrimitiveType::Void:
      buffer += "void";
      break;
    case Type::PrimitiveType::Integer:
      buffer += fmt::format("i{}", ty.width);
      break;
    }
    buffer += std::string(ty.pointer, '*');
    return buffer;
  }

  static std::string dumpUser(Value *value) {
    return join(
      value->getImmutableEdges(), ListIterator<UseEdge>(),
      [](auto edge) { return fmt::format("%{}", edge.to->getIdentity()); },
      " ");
  }

  static std::string
  dumpFunctionParameterList(const std::vector<Argument *> &param) {
    return join(
      param.begin(), param.end(),
      [](auto arg) {
        return fmt::format("#{}: {} /* {} */", arg->name, dumpType(arg->type),
                           dumpUser(arg));
      },
      ", ");
  }

  static std::string dumpInstructionInput(Value *value) {
    if (value == nullptr)
      return "<nullptr>";
    if (auto *ci = value->as<ConstantInteger *>())
      return fmt::format("{}", ci->value);
    if (auto *insn = value->as<Instruction *>())
      return fmt::format("{} %{}", dumpType(insn->type), insn->getIdentity());
    if (auto *arg = value->as<Argument *>())
      return fmt::format("{} #{}", dumpType(arg->type), arg->name);
    if (auto *gv = value->as<GlobalVariable *>())
      return fmt::format("{} @{}.addr", dumpType(gv->type), gv->name);
    if (auto *bb = value->as<BasicBlock *>())
      return fmt::format("label L{}", bb->getIdentity());
    if (auto *f = value->as<Function *>())
      return fmt::format("fn {} {}", dumpType(f->return_type), f->name);
    throw std::runtime_error("can not dump this SSAValue");
  }

  void dumpAllGlobalVariable(IRHost &host) {
    std::for_each(host.getModule()->global.begin(),
                  host.getModule()->global.end(), [&](auto gv) {
                    buffer += fmt::format("@{}.addr : {} /* {} */\n", gv->name,
                                          dumpType(gv->type), dumpUser(gv));
                  });
    buffer += "\n";
  }

  void dumpInstruction(Instruction *insn) {
    buffer += fmt::format("    {} %{} <- {} ", dumpType(insn->type),
                          insn->getIdentity(), op_name[insn->op]);
    buffer += join(
      insn->getInput().begin(), insn->getInput().end(),
      [this](auto v) { return dumpInstructionInput(v.from); }, ", ");
    buffer += fmt::format(" /* {} */\n", dumpUser(insn));
  }

  void dumpBasicBlock(BasicBlock *bb) {
    buffer += fmt::format("L{}:\n", bb->getIdentity());
    std::for_each(bb->begin(), bb->end(),
                  [&](auto insn) { dumpInstruction(&insn); });
  }

  void dumpFunction(Function *func) {
    buffer += fmt::format("fn {} ({}) -> {}", func->name,
                          dumpFunctionParameterList(func->arg),
                          dumpType(func->return_type));
    if (func->external)
      buffer += ";";
    else {
      buffer += " {\n";
      std::for_each(func->block.begin(), func->block.end(),
                    [&](auto bb) { dumpBasicBlock(&bb); });
      buffer += "}";
    }
    buffer += "\n\n";
  }

  void assignIdentity(IRHost &host) {
    std::for_each(
      host.getModule()->func.begin(), host.getModule()->func.end(),
      [&](auto &f) {
        std::for_each(f->block.begin(), f->block.end(), [&](auto &bb) {
          bb.getIdentity() = ++id;
          std::for_each(bb.begin(), bb.end(),
                        [&](auto &insn) { insn.getIdentity() = ++id; });
        });
      });
  }

  void dumpIRText(IRHost &host) {
    assignIdentity(host);
    dumpAllGlobalVariable(host);
    std::for_each(host.getModule()->func.begin(), host.getModule()->func.end(),
                  [&](auto f) { dumpFunction(f); });
    static int ir_count = 0;
    auto out = fmt::output_file(fmt::format("dump.new-ir.{}.txt", ir_count++));
    out.print("{}", buffer);
  }

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