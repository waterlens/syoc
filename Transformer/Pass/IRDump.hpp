#pragma once
#include "IR/IR.hpp"
#include "Tree/Tree.hpp"
#include "fmt/format.h"
#include "fmt/os.h"

#include <cassert>
#include <cstddef>
#include <stdexcept>
#include <string>
#include <string_view>

class IRDump {
  std::string buffer;

  static std::string dumpSSATypeOld(const SSAType &ty) {
    std::string buffer;
    if (!ty.dim.empty()) {
      auto first = ty.dim.front();
      auto new_ty = ty;
      new_ty.dim.pop_front();
      new_ty.pointer = 0;
      buffer += fmt::format("[{} x {}]", first, dumpSSAType(new_ty));
    } else
      switch (ty.primitive_type) {
      case SSAType::PrimitiveType::Void:
        buffer += "void";
        break;
      case SSAType::PrimitiveType::Integer:
        buffer += fmt::format("i{}", ty.width);
        break;
      }
    buffer += std::string(ty.pointer, '*');
    return buffer;
    throw std::runtime_error("can not dump this SSAType");
  }

  static std::string dumpSSAType(const SSAType &ty) {
    std::string buffer;
    switch (ty.primitive_type) {
    case SSAType::PrimitiveType::Void:
      buffer += "void";
      break;
    case SSAType::PrimitiveType::Integer:
      buffer += fmt::format("i{}", ty.width);
      break;
    }
    buffer += std::string(ty.pointer, '*');
    return buffer;
    throw std::runtime_error("can not dump this SSAType");
  }

  void dumpCFG(IRHost &host) {
    buffer.clear();
    auto add_edge = [&](SSAValueHandle from, SSAValueHandle to,
                        std::string_view label = "") {
      buffer +=
        fmt::format("  node_{} -> node_{} [label=\"{}\"];\n", from, to, label);
    };
    auto add_node = [&](SSAValueHandle node, std::string_view label) {
      buffer += fmt::format("  node_{} [label=\"{}\"];\n", node, label);
    };
    auto bb_printer = [&](BasicBlock *bb) {
      static std::string small_str(16, 0);
      assert(bb != nullptr);
      if (!bb->insn.empty()) {
        auto value = host[bb->insn.back()];
        if (auto *insn = host[bb->insn.back()].as<Instruction *>()) {
          if (insn->op == OP_Return) {
            small_str = fmt::format("L{} exit", bb->identity);
            add_node(bb->identity, small_str);
          } else if (insn->op == OP_Branch) {
            small_str = fmt::format("L{}", bb->identity);
            add_node(bb->identity, small_str);
            add_edge(bb->identity, insn->args[1]);
            add_edge(bb->identity, insn->args[2]);
          } else if (insn->op == OP_Jump) {
            small_str = fmt::format("L{}", bb->identity);
            add_node(bb->identity, small_str);
            add_edge(bb->identity, insn->args[0]);
          } else
            throw std::runtime_error("last of the basic block is not a "
                                     "terminator");
        } else
          throw std::runtime_error(
            "last of the basic block is not an instruction");
      }
      else {
        small_str = fmt::format("L{} empty", bb->identity);
        add_node(bb->identity, small_str);
      }
    };
    for (auto &&handle : host.function_table) {
      auto *func = host[handle].as<Function *>();
      if (func->basic_block.empty())
        continue;
      add_node(handle, fmt::format("Function {}", func->name));
      add_edge(handle, func->basic_block.front());
      for (auto &&bb : func->basic_block) {
        bb_printer(host[bb].as<BasicBlock *>());
      }
    }

    auto out = fmt::output_file("dump.cfg.dot");
    out.print(
      R"(digraph CFG {{
  node [shape=box];
{}
}})",
      buffer);
  }

  void dumpIRText(IRHost &host) {
    buffer.clear();
    for (auto &&handle : host.global_value_table) {
      auto *gv = host[handle].as<GlobalVariable *>();
      buffer += fmt::format("@{}.addr %{}: {}\n", gv->name, gv->identity,
                            dumpSSAType(gv->type));
    }
    auto func_arg_printer = [this,
                             &host](const std::vector<SSAValueHandle> &args) {
      std::string buffer;
      for (auto iter = args.cbegin(); iter != args.cend(); ++iter) {
        if (iter != args.cbegin())
          buffer += ", ";
        auto *arg = host[*iter].as<Argument *>();
        buffer += fmt::format("{}: {}", arg->name, dumpSSAType(arg->type));
      }
      return buffer;
    };
    auto insn_arg_printer = [this, &host](SSAValueHandle handle) {
      if (!handle.isValid())
        return std::string();
      if (auto *ci = host[handle].as<ConstantInteger *>())
        return fmt::format("{}", ci->value);
      if (auto *insn = host[handle].as<Instruction *>())
        return fmt::format("{} %{}", dumpSSAType(insn->type), insn->identity);
      if (auto *arg = host[handle].as<Argument *>())
        return fmt::format("{} {}", dumpSSAType(arg->type), arg->name);
      if (auto *gv = host[handle].as<GlobalVariable *>())
        return fmt::format("{} %{}", dumpSSAType(gv->type), gv->identity);
      if (auto *bb = host[handle].as<BasicBlock *>())
        return fmt::format("label L{}", bb->identity);
      if (auto *f = host[handle].as<Function *>())
        return fmt::format("fn {} {}", dumpSSAType(f->return_type), f->name);
      throw std::runtime_error("can not dump this SSAValue");
    };
    buffer += "\n";
    for (auto &&handle : host.function_table) {
      auto *func = host[handle].as<Function *>();
      buffer += fmt::format("fn {} %{} ({}) -> {}", func->name, func->identity,
                            func_arg_printer(func->args),
                            dumpSSAType(func->return_type));
      if (func->external)
        buffer += ";\n\n";
      else {
        buffer += " {\n";
        for (auto &&bb_handle : func->basic_block) {
          auto *bb = host[bb_handle].as<BasicBlock *>();
          buffer += fmt::format("L{}:\n", bb->identity);
          for (auto &&inst_handle : bb->insn) {
            auto *inst = host[inst_handle].as<Instruction *>();
            buffer += fmt::format("    {} %{} <- {} ", dumpSSAType(inst->type),
                                  inst->identity, op_name[inst->op]);
            for (const auto *iter = inst->args.cbegin();
                 iter != inst->args.cend(); ++iter) {
              if (!iter->isValid())
                break;
              if (iter != inst->args.cbegin())
                buffer += ", ";
              buffer += insn_arg_printer(*iter);
            }
            buffer += "\n";
          }
        }
        buffer += "}\n\n";
      }
    }

    auto out = fmt::output_file("dump.ir.txt");
    out.print("{}", buffer);
  }

public:
  IRDump() = default;
  void operator()(IRHost &host) {
    buffer.reserve(static_cast<std::size_t>(256 * 1024));
    dumpIRText(host);
    dumpCFG(host);
  }
};