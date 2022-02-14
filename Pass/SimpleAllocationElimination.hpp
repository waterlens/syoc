#pragma once

#include "IR/IR.hpp"
#include "Pass/Dump.hpp"
#include "Pass/IDominatorAnalysis.hpp"
#include "Pass/UseAnalysis.hpp"
#include "PassBase.hpp"
#include "Tree/Tree.hpp"
#include "Util/Filter.hpp"
#include "Util/TrivialValueVector.hpp"
#include <algorithm>
#include <unordered_map>
#include <vector>

class SimpleAllocationElimination
  : public SSATransformation<SimpleAllocationElimination> {
private:
  static void eliminateDeadAllocation(IRHost &host) {
    for (auto &&f_handle : host.getValidFunction()) {
      auto *f = host[f_handle].as<Function *>();
      if (f->external)
        continue;
      auto *alloca_bb = host[f->getValidBasicBlockFront()].as<BasicBlock *>();
      for (auto &&insn_handle : alloca_bb->getValidInstruction()) {
        auto *alloca = host[insn_handle].as<Instruction *>();
        if (alloca->op != OP_Allocate)
          continue;
        if (alloca->getValidUser().size() == 0) {
          insn_handle = SSAValueHandle::InvalidValueHandle();
          continue;
        }
      }
    }
  }

  static void eliminateSingleDefinitionAllocation(IRHost &host) {
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
        auto *alloca = host[insn_handle].as<Instruction *>();
        if (alloca->op != OP_Allocate || alloca->getValidUser().size() == 0)
          continue;

        TrivialValueVector<SSAValueHandle *, 8> defs;
        bool skip = false;
        for (auto &&user_handle : alloca->getValidUser()) {
          auto *insn = host[user_handle].as<Instruction *>();
          if (insn->op == OP_Store || insn->op == OP_Memset0)
            defs.push_back(&user_handle); // definition
          else if (insn->op == OP_Offset || insn->op == OP_Call) {
            skip = true;
            break; // do not touch those allocation used by offset
          }
        }
        if (skip)
          continue;

        if (defs.size() == 1) {
          auto &def_handle = *defs[0]; // the only definition
          auto *def_insn = host[def_handle].as<Instruction *>();
          if (def_insn->op == OP_Store) { // it's a store rather than a memset0
            auto def_bb_handle = def_insn->parent;
            auto store_source = def_insn->args[1];
            auto all_dominated = lazy_idom().findAllDominatedSet(def_bb_handle);
            auto user_count =
              alloca->getValidUser().size() - 1; // except the store
            size_t replace_count = 0;

            for (auto &&load_handle : alloca->getValidUser()) {
              if (auto *load = host[load_handle].as<Instruction *>()) {
                if (load->op == OP_Load)
                  // the load inst occurs in a dominated block but not the
                  // definition block
                  if (all_dominated.count(load->parent) > 0 &&
                      load->parent != alloca_bb->identity) {
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
              if (alloca->getValidUser().size() == 0)
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

    UseAnalysis{}(host);
  }

  static void eliminateSingleBlockAllocation(IRHost &host) {
    SSAValueHandle latest_store;
    std::vector<SSAValueHandle *> store_remove;

    for (auto &&f_handle : host.getValidFunction()) {
      auto *f = host[f_handle].as<Function *>();
      if (f->external)
        continue;
      auto *alloca_bb = host[f->getValidBasicBlockFront()].as<BasicBlock *>();
      for (auto &&alloca_handle : alloca_bb->getValidInstruction()) {
        auto *alloca = host[alloca_handle].as<Instruction *>();
        if (alloca->op != OP_Allocate || alloca->getValidUser().size() == 0)
          continue;

        TrivialValueVector<Instruction *, 8> load_store_uses;
        bool skip = false;
        for (auto &&user_handle : alloca->getValidUser()) {
          auto *insn = host[user_handle].as<Instruction *>();
          if (insn->op == OP_Store || insn->op == OP_Load)
            load_store_uses.push_back(insn);
          else if (insn->op == OP_Offset || insn->op == OP_Memset0 ||
                   insn->op == OP_Call) {
            skip = true;
            break; // do not touch those allocation
          }
        }

        if (skip || load_store_uses.empty())
          continue;

        auto parent = load_store_uses.front()->parent;
        bool all_in_one_block =
          std::all_of(load_store_uses.begin(), load_store_uses.end(),
                      [&](auto *insn) { return insn->parent == parent; });
        if (!all_in_one_block)
          continue;

        auto *bb = host[parent].as<BasicBlock *>();
        latest_store = SSAValueHandle::InvalidValueHandle();
        store_remove.clear();

        for (auto &&insn_handle : bb->getValidInstruction()) {
          auto *insn = host[insn_handle].as<Instruction *>();
          if (insn->op == OP_Store && insn->args[0] == alloca->identity) {
            // record the latest value of store
            latest_store = insn->args[1];
            // the store should be removed in the future
            store_remove.push_back(&insn_handle);
          } else if (insn->op == OP_Offset || insn->op == OP_Memset0 ||
                     insn->op == OP_Call) {
            if (insn->op != OP_Call && insn->args[0] == alloca->identity) {
              latest_store = SSAValueHandle::InvalidValueHandle();
              if (!store_remove.empty())
                store_remove.pop_back();
            } else
              for (int i = 1; i < insn->args.size(); ++i) {
                if (insn->args[i] == alloca->identity) {
                  latest_store = SSAValueHandle::InvalidValueHandle();
                  if (!store_remove.empty())
                    store_remove.pop_back();
                  break;
                }
              }
          } else if (insn->op == OP_Load) {
            // check if the load can be replaced
            if (insn->args[0] == alloca->identity &&
                latest_store != SSAValueHandle::InvalidValueHandle()) {
              // remove instruction from bb
              bb->removeInstructionInFuture(insn_handle);
              // replace the load instruction with the latest store value
              host.replace(insn_handle, latest_store);
            }
          }
        }

        for (auto &&store_insn : store_remove)
          bb->removeInstructionInFuture(*store_insn);
        bb->removeInstruction();
        alloca_handle = SSAValueHandle::InvalidValueHandle();
      }

      for (auto &&bb_handle : f->getValidBasicBlock()) {
        auto *bb = host[bb_handle].as<BasicBlock *>();
        bb->removeInstruction();
      }
    }
    UseAnalysis{}(host);
  }

public:
  SimpleAllocationElimination() = default;
  [[nodiscard]] static std::string_view getName() {
    return "Simple Allocation Elimination";
  }
  void operator()(IRHost &host) {
    eliminateDeadAllocation(host);
    eliminateSingleDefinitionAllocation(host);
    eliminateSingleBlockAllocation(host);
    eliminateDeadAllocation(host);
  }
};