#include "IR/IR.hpp"
#include "Tree/Tree.hpp"
#include "fmt/format.h"
#include "fmt/os.h"
#include <stdexcept>
#include <string>

class IRDump {
  std::string buffer;

  std::string dumpSSATypeOld(const SSAType &ty) {
    std::string buffer;
    if (ty.dim.size()) {
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

  std::string dumpSSAType(const SSAType &ty) {
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

  void dumpIRText(IRHost &host) {
    buffer.reserve(256 * 1024);
    buffer.clear();
    for (auto &&handle : host.global_value_table) {
      auto gv = host[handle].as<GlobalVariable *>();
      buffer += fmt::format("@{}.addr %{}: {}\n", gv->name, gv->identity,
                            dumpSSAType(gv->type));
    }
    auto func_arg_printer = [this,
                             &host](const std::vector<SSAValueHandle> &args) {
      std::string buffer;
      for (auto iter = args.cbegin(); iter != args.cend(); ++iter) {
        if (iter != args.cbegin())
          buffer += ", ";
        auto arg = host[*iter].as<Argument *>();
        buffer += fmt::format("{}: {}", arg->name, dumpSSAType(arg->type));
      }
      return buffer;
    };
    auto insn_arg_printer = [this, &host](SSAValueHandle handle) {
      if (!handle.isValid())
        return std::string();
      if (auto ci = host[handle].as<ConstantInteger *>())
        return fmt::format("{}", ci->value);
      else if (auto insn = host[handle].as<Instruction *>())
        return fmt::format("{} %{}", dumpSSAType(insn->type), insn->identity);
      else if (auto arg = host[handle].as<Argument *>())
        return fmt::format("{} {}", dumpSSAType(arg->type), arg->name);
      else if (auto gv = host[handle].as<GlobalVariable *>())
        return fmt::format("{} %{}", dumpSSAType(gv->type), gv->identity);
      else if (auto bb = host[handle].as<BasicBlock *>())
        return fmt::format("label L{}", bb->identity);
      else if (auto f = host[handle].as<Function *>())
        return fmt::format("fn {} {}", dumpSSAType(f->return_type), f->name);
      else
        throw std::runtime_error("can not dump this SSAValue");
    };
    buffer += "\n";
    for (auto &&handle : host.function_table) {
      auto func = host[handle].as<Function *>();
      buffer += fmt::format("fn {} %{} ({}) -> {}", func->name, func->identity,
                            func_arg_printer(func->args),
                            dumpSSAType(func->return_type));
      if (func->external)
        buffer += ";\n\n";
      else {
        buffer += " {\n";
        for (auto &&bb_handle : func->basic_block) {
          auto bb = host[bb_handle].as<BasicBlock *>();
          buffer += fmt::format("L{}:\n", bb->identity);
          for (auto &&inst_handle : bb->insn) {
            auto inst = host[inst_handle].as<Instruction *>();
            buffer += fmt::format("    {} %{} <- {} ", dumpSSAType(inst->type),
                                  inst->identity, op_name[inst->op]);
            for (auto iter = inst->args.cbegin(); iter != inst->args.cend();
                 ++iter) {
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
  IRDump(){};
  void operator()(IRHost &host) { dumpIRText(host); }
};