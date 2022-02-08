#pragma once

#include "Pass/PassBase.hpp"

#include <cassert>
#include <stdexcept>

class BBPredSuccAnalysis final : public SSAAnalysis<BBPredSuccAnalysis> {
public:
  BBPredSuccAnalysis() = default;
  [[nodiscard]] static std::string_view getName()  {
    return "Basic Block Predecessor and Successor Analysis";
  }
  void operator()(IRHost &host) {
    for (auto &&handle : host.function_table) {
      auto *func = host[handle].as<Function *>();
      for (auto &&bb_handle : func->basic_block) {
        auto *bb = host[bb_handle].as<BasicBlock *>();

        bb->pred.clear();
        bb->succ.clear();

        if (!bb->insn.empty()) {
          auto value = host[bb->insn.back()];
          if (auto *insn = host[bb->insn.back()].as<Instruction *>()) {
            if (insn->op == OP_Return) {
              // do nothing
            } else if (insn->op == OP_Branch) {
              auto *tbb1 = host[insn->args[1]].as<BasicBlock *>();
              auto *tbb2 = host[insn->args[2]].as<BasicBlock *>();
              assert(tbb1 != nullptr && tbb2 != nullptr);

              tbb1->pred.push_back(bb->identity);
              tbb2->pred.push_back(bb->identity);
              bb->succ.push_back(tbb1->identity);
              bb->succ.push_back(tbb2->identity);

            } else if (insn->op == OP_Jump) {
              auto *tbb = host[insn->args[0]].as<BasicBlock *>();
              assert(tbb != nullptr);

              tbb->pred.push_back(bb->identity);
              bb->succ.push_back(tbb->identity);

            } else
              throw std::runtime_error("last of the basic block is not a "
                                       "terminator");
          } else
            throw std::runtime_error(
              "last of the basic block is not an instruction");
        } else {
        }
      }
    }
  }
};