#pragma once

#include "IR/IR.hpp"
#include "Pass/IDominatorAnalysis.hpp"
#include "Pass/UseAnalysis.hpp"
#include "PassBase.hpp"
#include "Tree/Tree.hpp"
#include "Util/Filter.hpp"
#include "Util/TrivialValueVector.hpp"
#include <vector>

class SimpleAllocationElimination
  : public SSATransformation<SimpleAllocationElimination> {
private:
  static void eliminateSingleOrNoDefinitionAllocation(IRHost &host) {
    IDominatorAnalysis idom;
    bool idom_initialized = false;
    auto lazy_idom = [&]() -> auto & {
      if (!idom_initialized) {
        idom(host);
        idom_initialized = true;
      }
      return idom;
    };
    for (auto &&f_handle : host.getValidFunction()) {
      auto *f = host[f_handle].as<Function *>();
      if (f->external)
        continue;
      auto *alloca_bb = host[f->getValidBasicBlockFront()].as<BasicBlock *>();
      for (auto &&insn_handle : alloca_bb->getValidInstruction()) {
        auto *insn = host[insn_handle].as<Instruction *>();
        if (insn->op != OP_Allocate)
          continue;
        if (insn->getValidUser().size() == 0) {
          insn_handle = SSAValueHandle::InvalidValueHandle();
          continue;
        }

        TrivialValueVector<SSAValueHandle *, 8> defs;
        for (auto &&user_handle : insn->getValidUser()) {
          auto *insn = host[user_handle].as<Instruction *>();
          if (insn->op == OP_Store || insn->op == OP_Memset0)
            defs.push_back(&user_handle); // definition
        }

        if (defs.size() == 1) {
          auto &def_handle = *defs[0]; // the only definition
          auto *def_insn = host[def_handle].as<Instruction *>();
          if (def_insn->op == OP_Store) { // it's a store rather than a memset0
            auto def_bb_handle = def_insn->parent;
            auto store_source = def_insn->args[1];
            auto all_dominated = lazy_idom().findAllDominatedSet(def_bb_handle);
            auto user_count =
              insn->getValidUser().size() - 1; // except the store
            size_t replace_count = 0;

            for (auto &&load_handle : insn->getValidUser()) {
              if (auto *load = host[load_handle].as<Instruction *>()) {
                if (load->op == OP_Load)
                  // the load inst occurs in a dominated block but not the
                  // definition block
                  if (all_dominated.count(load->parent) > 0 &&
                      load->parent != def_bb_handle) {
                    // remove the load
                    auto *load_bb = host[load->parent].as<BasicBlock *>();
                    load_bb->removeInstructionInFuture(load_handle);
                    // replace load with the store source
                    host.replace(load_handle, store_source);
                    // the alloca is not used
                    load_handle = SSAValueHandle::InvalidValueHandle();
                    ++replace_count;
                  }
              }
            }
            // all users are replaced with the store source
            if (replace_count == user_count) {
              // remove the store from its basic block
              auto *def_bb = host[def_bb_handle].as<BasicBlock *>();
              def_bb->removeInstructionInFuture(def_handle);
              def_handle = SSAValueHandle::InvalidValueHandle();
              if (insn->getValidUser().size() == 0)
                insn_handle = SSAValueHandle::InvalidValueHandle();
            }
          }
        }
      }
      for (auto &&bb_handle : f->getValidBasicBlock()) {
        auto *bb = host[bb_handle].as<BasicBlock *>();
        bb->removeInstruction();
      }
    }
  }
  static void eliminateLocalLoadStore(IRHost &host);

public:
  SimpleAllocationElimination() = default;
  [[nodiscard]] static std::string_view getName() {
    return "Simple Allocation Elimination";
  }
  void operator()(IRHost &host) {
    eliminateSingleOrNoDefinitionAllocation(host);
  }
};