#pragma once

#include "IR/IR.hpp"
#include "Pass/UseAnalysis.hpp"
#include "PassBase.hpp"
#include "Tree/Tree.hpp"
#include "Util/Filter.hpp"
#include "Util/TrivialValueVector.hpp"
#include <unordered_map>
#include <vector>

class LocalCopyPropagation : public SSATransformation<LocalCopyPropagation> {
private:
  // local copy propagation
  static void eliminateLocalLoad(IRHost &host) {
    std::unordered_map<SSAValueHandle, SSAValueHandle> latest_store;

    auto helper = [&](Instruction *insn, unsigned idx) {
      latest_store.erase(insn->args[idx]);
    };

    for (auto &&f_handle : host.getValidFunction()) {
      auto *f = host[f_handle].as<Function *>();
      if (f->external)
        continue;

      for (auto &&bb_handle : f->getValidBasicBlock()) {
        auto *bb = host[bb_handle].as<BasicBlock *>();
        latest_store.clear();

        for (auto &&insn_handle : bb->getValidInstruction()) {
          auto *insn = host[insn_handle].as<Instruction *>();
          if (insn->op == OP_Store) {
            if (host[insn->args[0]].is<GlobalVariable *>())
              continue;
            // record the latest value of store
            latest_store[insn->args[0]] = insn->args[1];
          } else if (insn->op == OP_Offset || insn->op == OP_Memset0 ||
                     insn->op == OP_Call) {
            if (insn->op != OP_Call)
              helper(insn, 0);
            else
              for (int i = 1; i < insn->args.size(); ++i) helper(insn, i);
          } else if (insn->op == OP_Load) {
            // check if the load can be replaced
            if (latest_store.contains(insn->args[0])) {
              // remove instruction from bb
              bb->removeInstructionInFuture(insn_handle);
              // replace the load instruction with the latest store value
              host.replace(insn_handle, latest_store[insn->args[0]]);
            }
          }
        }

        bb->removeInstruction();
        UseAnalysis{}(host);
      }
    }
  }

public:
  LocalCopyPropagation() = default;
  [[nodiscard]] static std::string_view getName() {
    return "Local Copy Propagation";
  }
  void operator()(IRHost &host) { eliminateLocalLoad(host); }
};