#include "IR/IR.hpp"
#include "PassCollection.hpp"
#include "Tree/Tree.hpp"
#include "Util/Filter.hpp"
#include <cassert>
#include <limits>
#include <stdexcept>
#include <string>
#include <unordered_set>

void BBPredSuccAnalysis::operator()(IRHost &host) {
  for (auto &&handle : host.getValidFunction()) {
    auto *func = host[handle].as<Function *>();
    for (auto &&bb_handle : func->getValidBasicBlock()) {
      auto *bb = host[bb_handle].as<BasicBlock *>();
      bb->pred.clear();
      bb->succ.clear();
    }

    for (auto &&bb_handle : func->getValidBasicBlock()) {
      auto *bb = host[bb_handle].as<BasicBlock *>();

      if (!bb->getValidInstruction().empty()) {
        if (auto *insn =
              host[bb->getValidInstructionBack()].as<Instruction *>()) {
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
        throw std ::runtime_error("basic block is empty");
      }
    }
  }
}

void BasicBlockTraversalAnalysis::operator()(IRHost &host) { p_host = &host; }

void IRDump::operator()(IRHost &host) {
  buffer.reserve(static_cast<std::size_t>(256 * 1024));
  dumpIRText(host);
}

void CFGDump::operator()(IRHost &host) { dumpCFG(host); }

void IDominatorDump::operator()(IRHost &host) { dumpIDominator(host); }

void IDominatorAnalysis::operator()(IRHost &host) {
  BasicBlockTraversalAnalysis bb_traversal;
  bb_traversal(host);
  for (auto &&handle : host.getValidFunction()) {
    auto *func = host[handle].as<Function *>();
    if (!func->getValidBasicBlock().empty())
      calculateImmediateDominator(host, bb_traversal, func);
  }
}

std::string IRDump::dumpSSATypeOld(const SSAType &ty) {
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

std::string IRDump::dumpSSAType(const SSAType &ty) {
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

void CFGDump::dumpCFG(IRHost &host) {
  GraphHelper cfg;
  auto bb_printer = [&](BasicBlock *bb) {
    static std::string small_str(16, 0);
    assert(bb != nullptr);
    if (!bb->getValidInstruction().empty()) {
      auto value = host[bb->getValidInstructionBack()];
      if (auto *insn =
            host[bb->getValidInstructionBack()].as<Instruction *>()) {
        if (insn->op == OP_Return) {
          cfg.addNode(bb->identity, fmt::format("L{} exit", bb->identity));
        } else if (insn->op == OP_Branch || insn->op == OP_Jump) {
          cfg.addNode(bb->identity, fmt::format("L{}", bb->identity));
        } else
          throw std::runtime_error("last of the basic block is not a "
                                   "terminator");
        for (auto &&succ : bb->succ) cfg.addEdge(bb->identity, succ, "");
      } else
        throw std::runtime_error(
          "last of the basic block is not an instruction");
    } else {
      cfg.addNode(bb->identity, fmt::format("L{} empty", bb->identity));
    }
  };

  for (auto &&handle : host.getValidFunction()) {
    auto *func = host[handle].as<Function *>();
    if (func->external)
      continue;
    cfg.addNode(handle, fmt::format("Function {}", func->name));
    cfg.addEdge(handle, func->getValidBasicBlockFront(), "");
    for (auto &&bb : func->getValidBasicBlock())
      bb_printer(host[bb].as<BasicBlock *>());
  }

  static int cfg_count = 0;
  cfg.outputToFile(fmt::format("dump.cfg.{}.dot", cfg_count++), "CFG");
}

void IRDump::dumpIRText(IRHost &host) {
  buffer.clear();

  auto user_printer = [&](const SSAValue &v) {
    std::string tmp;
    const auto &valid_user = const_filter(v.user, handleIsValid);
    for (auto iter = valid_user.cbegin(); iter != valid_user.cend(); ++iter) {
      if (iter != valid_user.cbegin())
        tmp += " ";
      tmp += fmt::format("%{}", iter->id);
    }
    return tmp;
  };

  auto func_arg_printer = [&](const std::vector<SSAValueHandle> &args) {
    std::string buffer;
    for (auto iter = args.cbegin(); iter != args.cend(); ++iter) {
      if (iter != args.cbegin())
        buffer += ", ";
      auto *arg = host[*iter].as<Argument *>();
      buffer += fmt::format("{}: {} /* {} */", arg->name,
                            dumpSSAType(arg->type), user_printer(host[*iter]));
    }
    return buffer;
  };

  auto insn_arg_printer = [&](SSAValueHandle handle) {
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

  for (auto &&handle : host.getValidGlobalVariable()) {
    auto *gv = host[handle].as<GlobalVariable *>();
    buffer += fmt::format("@{}.addr %{}: {} /* {} */\n", gv->name, gv->identity,
                          dumpSSAType(gv->type), user_printer(host[handle]));
  }

  buffer += "\n";
  for (auto &&handle : host.getValidFunction()) {
    auto *func = host[handle].as<Function *>();
    buffer +=
      fmt::format("fn {} %{} ({}) -> {}", func->name, func->identity,
                  func_arg_printer(func->args), dumpSSAType(func->return_type));
    if (func->external)
      buffer += ";\n\n";
    else {
      buffer += " {\n";
      for (auto &&bb_handle : func->getValidBasicBlock()) {
        auto *bb = host[bb_handle].as<BasicBlock *>();
        buffer += fmt::format("L{}:\n", bb->identity);
        for (auto &&inst_handle : bb->getValidInstruction()) {
          auto *inst = host[inst_handle].as<Instruction *>();
          buffer += fmt::format("    {} %{} <- {} ", dumpSSAType(inst->type),
                                inst->identity, op_name[inst->op]);
          for (const auto *iter = inst->args.cbegin();
               iter != inst->args.cend(); ++iter) {
            if (iter != inst->args.cbegin())
              buffer += ", ";
            buffer += insn_arg_printer(*iter);
          }
          buffer += fmt::format(" /* {} */\n", user_printer(host[inst_handle]));
        }
      }
      buffer += "}\n\n";
    }
  }

  static int ir_count = 0;
  auto out = fmt::output_file(fmt::format("dump.ir.{}.txt", ir_count++));
  out.print("{}", buffer);
}

void IDominatorDump::dumpIDominator(IRHost &host) {
  IDominatorAnalysis ida;
  GraphHelper idg;
  ida(host);

  std::unordered_set<unsigned> visited;
  const auto &idominated_map = ida.getIDominatorMap().first;
  for (auto [idominated, idominator] : idominated_map) {
    if (visited.count(idominated) == 0) {
      idg.addNode(idominated, fmt::format("L{}", idominated));
      visited.insert(idominated);
    }
    if (visited.count(idominator) == 0) {
      idg.addNode(idominator, fmt::format("L{}", idominator));
      visited.insert(idominator);
    }
    idg.addEdge(idominator, idominated, "");
  }
  visited.clear();

  static int g_count = 0;
  idg.outputToFile(fmt::format("dump.idom.{}.dot", g_count), "IDominator");
}

std::vector<SSAValueHandle>
IDominatorAnalysis::findAllDominated(SSAValueHandle dominator) const {
  std::vector<SSAValueHandle> tmp;
  std::vector<SSAValueHandle> dominated;
  tmp.push_back(dominator);
  while (!tmp.empty()) {
    auto handle = tmp.back();
    tmp.pop_back();
    auto range = immediate_dominating_map.equal_range(handle);
    for (auto iter = range.first; iter != range.second; ++iter) {
      dominated.push_back({iter->second});
      tmp.push_back({iter->second});
    }
  }
  dominated.push_back(dominator); // always dominates itself
  return dominated;
}

std::unordered_set<SSAValueHandle>
IDominatorAnalysis::findAllDominatedSet(SSAValueHandle dominator) const {
  std::vector<SSAValueHandle> tmp;
  std::unordered_set<SSAValueHandle> dominated;
  tmp.push_back(dominator);
  while (!tmp.empty()) {
    auto handle = tmp.back();
    tmp.pop_back();
    auto range = immediate_dominating_map.equal_range(handle);
    for (auto iter = range.first; iter != range.second; ++iter) {
      dominated.insert({iter->second});
      tmp.push_back({iter->second});
    }
  }
  dominated.insert(dominator); // always dominates itself
  return dominated;
}

void IDominatorAnalysis::calculateImmediateDominator(
  IRHost &host, BasicBlockTraversalAnalysis &bb_traversal, Function *f) {
  invalidateAllExtraId(host, f);
  bb_traversal.runOnFunction(f);
  auto rpo = bb_traversal.getPostfix();
  bool changed = true;
  std::reverse(rpo.begin(), rpo.end());
  int id = rpo.size();
  // extra_id is assigned by the postorder traversal
  for (auto &&handle : rpo) host[handle].as<BasicBlock *>()->extra_id = --id;
  // doms[i] = idom(i), doms[i] and i is the extra_id, not the index of rpo!
  std::vector<int> doms(rpo.size(), -1);
  // doms[start_node] = start_node
  doms[rpo.size() - 1] = rpo.size() - 1;

  while (changed) {
    changed = false;
    // skip the start node, in the reverse post order
    for (int i = 1; i < rpo.size(); ++i) {
      auto bb_handle = rpo[i];
      auto *bb = host[bb_handle].as<BasicBlock *>();
      auto cur_id = bb->extra_id;

      assert(cur_id != std::numeric_limits<decltype(cur_id)>::max());
      assert(!bb->getValidPredecessor().empty());
      int new_idom =
        host[bb->getValidPredecessor().front()].as<BasicBlock *>()->extra_id;
      assert(new_idom != std::numeric_limits<decltype(cur_id)>::max());
      for (auto iter = ++bb->getValidPredecessor().cbegin();
           iter != bb->getValidPredecessor().cend(); ++iter) {
        auto other_pred_id = host[*iter].as<BasicBlock *>()->extra_id;
        assert(other_pred_id != std::numeric_limits<decltype(cur_id)>::max());
        if (doms[other_pred_id] != -1)
          new_idom = intersect(doms, other_pred_id, new_idom);
      }
      if (doms[cur_id] != new_idom) {
        doms[cur_id] = new_idom;
        changed = true;
      }
    }
  }
  auto doms_size = doms.size();
  for (int i = 0; i < doms_size - 1;
       ++i) { // up bound is not doms_size, because last one is pointed to
              // itself
    auto rpo_idx = doms_size - i - 1;
    immediate_dominated_map[rpo[rpo_idx]] = rpo[doms_size - doms[i] - 1];
    immediate_dominating_map.emplace(rpo[doms_size - doms[i] - 1],
                                     rpo[rpo_idx]);
  }
}

void ValuePoolCompact::operator()(IRHost &host) {
  auto &values = host.pool.values;
  std::vector<unsigned> remap(values.size(),
                              SSAValueHandle::InvalidValueHandle());
  // TODO
  assert(0);
}

void BasicBlockTraversalAnalysis::dfs(BasicBlock *bb) {
  auto &host = *p_host;
  bb->visited = true;
  prefix.push_back(bb->identity);
  for (auto &&succ_handle : bb->getValidSuccessor()) {
    auto *succ = host[succ_handle].as<BasicBlock *>();
    if (!succ->visited)
      dfs(succ);
  }
  postfix.push_back(bb->identity);
}

void BasicBlockTraversalAnalysis::clearVisitedFlag(Function *f) {
  if (f == nullptr || f->external)
    return;
  for (auto &&bb_handle : f->getValidBasicBlock()) {
    (*p_host)[bb_handle].as<BasicBlock *>()->visited = false;
  }
}

void BasicBlockTraversalAnalysis::runOnFunction(Function *f) {
  if (f == nullptr || f->external)
    return;
  clearVisitedFlag(f);
  prefix.clear();
  postfix.clear();
  auto *entry = (*p_host)[f->getValidBasicBlockFront()].as<BasicBlock *>();
  dfs(entry);
}

int IDominatorAnalysis::intersect(const std::vector<int> &doms, int pred,
                                  int now) {
  int m = pred;
  int n = now;
  while (m != n) {
    while (m < n) m = doms[m];
    while (n < m) n = doms[n];
  }
  return n;
}

void UseAnalysis::operator()(IRHost &host) {
  for (auto iter = host.pool.values.begin() + 1; iter != host.pool.values.end();
       ++iter)
    (*iter)->user.clear();
  for (auto &&handle : host.getValidFunction()) {
    auto *func = host[handle].as<Function *>();
    for (auto &&bb_handle : func->getValidBasicBlock()) {
      auto *bb = host[bb_handle].as<BasicBlock *>();
      for (auto &&insn_handle : bb->getValidInstruction()) {
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
            if (i % 2 == 0)
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

void SimplifyCFG::clearExtraJump(IRHost &host, BasicBlock *bb) {
  for (auto p = bb->insn.begin(); p != bb->insn.end(); ++p) {
    auto v = *p;
    if (auto *insn = host[v].as<Instruction *>()) {
      if (insn->op == OP_Jump || insn->op == OP_Branch) {
        bb->insn.resize(p - bb->insn.begin() + 1);
        break;
      }
    } else
      throw std::runtime_error("not a instruction");
  }
}

void SimplifyCFG::removeDanglingBB(IRHost &host) {
  for (auto &&func_handle : host.getValidFunction()) {
    auto *func = host[func_handle].as<Function *>();
    if (func->external)
      continue;
    for (auto &&bb_handle : func->getValidBasicBlock()) {
      auto *bb = host[bb_handle].as<BasicBlock *>();
      clearExtraJump(host, bb);
    }
  }
  BBPredSuccAnalysis{}(host);
  for (auto &&func_handle : host.getValidFunction()) {
    auto *func = host[func_handle].as<Function *>();
    if (func->external)
      continue;
    bool changed = true;
    while (changed) {
      changed = false;
      for (auto &&bb_handle : func->getValidBasicBlock()) {
        auto *bb = host[bb_handle].as<BasicBlock *>();
        if (bb->getValidPredecessor().empty() &&
            !bb->getValidSuccessor().empty() &&
            bb_handle.id != func->getValidBasicBlockFront().id) {
          // dangling basic block
          for (auto &&insn_handle : bb->getValidInstruction()) {
            switch (host[insn_handle].as<Instruction *>()->op) {
            case OP_Jump:
            case OP_Branch:
              break;
            default:
              throw std::runtime_error(
                "dangling basic block contains extra instructions");
            }
          }
          changed = true;
          for (auto succ_handle : bb->getValidSuccessor()) {
            auto *succ_bb = host[succ_handle].as<BasicBlock *>();
            succ_bb->pred.resize(std::remove(succ_bb->pred.begin(),
                                             succ_bb->pred.end(), bb_handle) -
                                 succ_bb->pred.begin());
          }
          bb->succ.clear();
          bb_handle = SSAValueHandle::InvalidValueHandle();
        }
      }
    }
  }
}

void SimplifyCFG::operator()(IRHost &host) { removeDanglingBB(host); }

void IDominatorAnalysis::invalidateAllExtraId(IRHost &host, Function *f) {
  if (f == nullptr || f->external)
    return;
  for (auto &&bb_handle : f->getValidBasicBlock()) {
    auto *bb = host[bb_handle].as<BasicBlock *>();
    bb->extra_id = std::numeric_limits<decltype(bb->extra_id)>::max();
  }
}
