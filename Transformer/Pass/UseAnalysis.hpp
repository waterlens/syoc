#pragma once

#include "IR/IR.hpp"

#include <cassert>
#include <stdexcept>

class UseAnalysis {
public:
  UseAnalysis() = default;
  void operator()(IRHost &host) {
    for (auto &&handle : host.function_table) {
      auto *func = host[handle].as<Function *>();
      for (auto &&bb_handle : func->basic_block) {
        auto *bb = host[bb_handle].as<BasicBlock *>();
        for (auto &&insn_handle : bb->insn) {
          auto *insn = host[insn_handle].as<Instruction *>();
          switch (insn->op) {
          case OP_Phi:
            throw std::runtime_error("Phi is not implemented");
          case OP_Mul:
          case OP_Div:
          case OP_Mod:
          case OP_Add:
          case OP_Sub:
          case OP_Lt:
          case OP_Gt:
          case OP_Le:
          case OP_Ge:
          case OP_Eq:
          case OP_Ne:
          case OP_Land:
          case OP_Lor:
          case OP_Lnot:
          case OP_Neg:
          case OP_Return:
          case OP_Load:
            for (auto &&handle : insn->args)
              host[handle].user.push_back(insn_handle);
            break;
          case OP_Branch:
          case OP_Memset0:
            host[insn->args[0]].user.push_back(insn_handle);
            break;
          case OP_Jump:
          case OP_Allocate:
            break;
          case OP_Call:
            for (unsigned i = 1; i < insn->args.size(); ++i)
              host[insn->args[i]].user.push_back(insn_handle);
            break;
          case OP_Offset:
            for (unsigned i = 0; i < insn->args.size(); ++i)
              if (i % 2 == 1)
                host[insn->args[i]].user.push_back(insn_handle);
            break;
          case OP_Store:
            host[insn->args[0]].user.push_back(insn_handle);
            host[insn->args[1]].user.push_back(insn_handle);
            break;
          case OP_None:
          case OP_End:
            assert(0);
          }
        }
      }
    }
  }
};