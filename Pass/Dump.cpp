#include "Dump.hpp"
#include "AllocaDefinitionHelper.hpp"
#include "AssignIdentityHelper.hpp"

namespace SyOC {
std::string IRDump::dumpType(const Type &ty) {
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

std::string IRDump::dumpUser(Value *value) {
  return fmt::format(
    "// {}",
    join(
      value->getEdgeHead(), ListIterator<UseEdge>(),
      [](auto &edge) { return fmt::format("%{}", edge.to->getIdentity()); },
      " "));
}

std::string
IRDump::dumpFunctionParameterList(const std::vector<Argument *> &param) {
  return join(
    param.begin(), param.end(),
    [](auto arg) {
      return fmt::format("#{}: {} {}", arg->name, dumpType(arg->type),
                         dumpUser(arg));
    },
    ", ");
}

std::string IRDump::dumpInstructionInput(Value *value) {
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

void IRDump::dumpAllGlobalVariable(IRHost &host) {
  std::for_each(host.getModule()->global.begin(),
                host.getModule()->global.end(), [&](GlobalVariable *gv) {
                  buffer +=
                    fmt::format("@{}.addr : {}, region size {} {}\n", gv->name,
                                dumpType(gv->type), gv->capacity, dumpUser(gv));
                });
  buffer += "\n";
}

void IRDump::dumpInstruction(Instruction *insn) {
  buffer += fmt::format("    {} %{} <- {} ", dumpType(insn->type),
                        insn->getIdentity(), op_name[insn->op]);
  buffer += join(
    insn->getInput().begin(), insn->getInput().end(),
    [this](auto &v) { return dumpInstructionInput(v.from); }, ", ");
  buffer += fmt::format(" {}\n", dumpUser(insn));
}

void IRDump::dumpBasicBlock(BasicBlock *bb) {
  buffer += fmt::format("L{}:\n", bb->getIdentity());
  std::for_each(bb->begin(), bb->end(),
                [&](auto &insn) { dumpInstruction(&insn); });
}

void IRDump::dumpFunction(Function *func) {
  buffer += fmt::format("fn {} ({}) -> {}", func->name,
                        dumpFunctionParameterList(func->arg),
                        dumpType(func->return_type));
  if (func->external)
    buffer += ";";
  else {
    buffer += " {\n";
    std::for_each(func->block.begin(), func->block.end(),
                  [&](auto &bb) { dumpBasicBlock(&bb); });
    buffer += "}";
  }
  buffer += "\n\n";
}

void IRDump::dumpIRText(IRHost &host) {
  assignIdentity(host);
  dumpAllGlobalVariable(host);
  std::for_each(host.getModule()->func.begin(), host.getModule()->func.end(),
                [&](auto f) { dumpFunction(f); });
  static int ir_count = 0;
  auto out = fmt::output_file(fmt::format("dump.new-ir.{}.txt", ir_count++));
  out.print("{}", buffer);
}

void CFGDump::dumpBasicBlock(GraphHelper &cfg, BasicBlock *bb) {
  static std::string small_str(16, 0);
  assert(bb != nullptr);

  assert(!bb->getInstruction().empty());
  auto &last = bb->getInstruction().back();
  assert(last.isControlInstruction());

  if (bb->isNormalBasicBlock())
    cfg.addNode(bb->getIdentity(), fmt::format("L{}", bb->getIdentity()));
  if (bb->isTerminatorBasicBlock())
    cfg.addNode(bb->getIdentity(), fmt::format("L{} exit", bb->getIdentity()));

  for (auto &pred : bb->getPredecessor())
    cfg.addEdge(pred.from->getIdentity(), pred.to->getIdentity(), "");
}

void CFGDump::operator()(IRHost &host) {
  GraphHelper cfg;
  assignIdentity(host);

  for (auto *func : host.getModule()->func) {
    if (func->external)
      continue;
    cfg.addNode(-func->getIdentity(), fmt::format("Function {}", func->name));
    cfg.addEdge(-func->getIdentity(), func->block.front().getIdentity(), "");
    for (auto &bb : func->block) dumpBasicBlock(cfg, &bb);
  }

  static int cfg_count = 0;
  cfg.outputToFile(fmt::format("dump.cfg.{}.dot", cfg_count++), "CFG");
}

void IDominatorDump::operator()(IRHost &host) {
  IDominatorAnalysis ida;
  GraphHelper idg;
  ida(host);
  assignIdentity(host);

  std::unordered_set<BasicBlock *> visited;
  const auto &idominated_map = ida.getIDominatorMap().first;
  for (auto [idominated, idominator] : idominated_map) {
    if (!visited.contains(idominated)) {
      idg.addNode(idominated->getIdentity(),
                  fmt::format("L{}", idominated->getIdentity()));
      visited.insert(idominated);
    }
    if (!visited.contains(idominator)) {
      idg.addNode(idominator->getIdentity(),
                  fmt::format("L{}", idominator->getIdentity()));
      visited.insert(idominator);
    }
    idg.addEdge(idominator->getIdentity(), idominated->getIdentity(), "");
  }
  visited.clear();

  static int g_count = 0;
  idg.outputToFile(fmt::format("dump.idom.{}.dot", g_count), "IDominator");
}

void IDFDump::operator()(IRHost &host) {
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

void IDFDump::dumpFunctionIDF(GraphHelper &cfg, Function &func,
                              IteratedDominanceFrontierAnalysis &idfa) {
  assert(!func.refExternal());

  std::unordered_set<BasicBlock *> visited;
  cfg.addNode(-func.getIdentity(), fmt::format("Function {}", func.name));

  auto &front = func.block.front();
  for (auto &insn : front) {
    if (insn.op == OP_Allocate &&
        insn.getInput()[0].from->as<SyOC::ConstantInteger *>()->value == 4) {
      cfg.addEdge(-func.getIdentity(), insn.getIdentity(), "");
      cfg.addNode(insn.getIdentity(),
                  fmt::format("var %{}", insn.getIdentity()));
      auto def_set = findDefinitionBlock(insn);
      for (auto *def : def_set) {
        if (!visited.contains(def)) {
          cfg.addNode(def->getIdentity(),
                      fmt::format("L{}", def->getIdentity()));
          visited.insert(def);
        }
        cfg.addEdge(insn.getIdentity(), def->getIdentity(), "Def");
      }
      auto [idf_set_split, idf_set] = idfa.getIDFSet(def_set);
      for (auto &[bb, idf] : idf_set_split) {
        if (!def_set.contains(bb))
          continue;
        for (auto *idf_elem : idf) {
          if (!visited.contains(idf_elem)) {
            cfg.addNode(idf_elem->getIdentity(),
                        fmt::format("L{}", idf_elem->getIdentity()));
            visited.insert(idf_elem);
          }
          cfg.addEdge(bb->getIdentity(), idf_elem->getIdentity(), "DF");
        }
      }
    }
  }
}
} // namespace SyOC