#include "PromoteMem2Reg.hpp"

namespace SyOC {

void PromoteMem2Reg::insertPhiForAlloca(IteratedDominanceFrontierAnalysis &idfa,
                                        Instruction &alloca) {
  assert(isPromotable(alloca));
  auto def_bb = findDefinitionBlock(alloca);
  auto [_, idf] = idfa.getIDFSet(def_bb);
  for (auto *bb : idf) {
    auto *phi = Instruction::create(OP_Phi, PredefinedType::Int32);
    phi->refParent() = bb;
    bb->getInstruction().push_front(phi);
    phiAllocaMap[phi] = &alloca;
    allocaPhiMap[&alloca].push_back(phi);
  }
}

void PromoteMem2Reg::renamePhiInSuccessorBasicBlock(BasicBlock &bb,
                                                    BasicBlock &pred) {
  for (auto &phi : bb.getInstruction()) {
    if (phi.op != OP_Phi)
      break;
    if (phiAllocaMap.count(&phi) == 0)
      throw std::runtime_error("phi is not associated with an allocation");
    auto *alloc = phiAllocaMap.at(&phi);
    if (containValue(&pred, alloc)) {
      phi.addInput(&pred);
      phi.addInput(getValue(&pred, alloc));
    }
  }
}

void PromoteMem2Reg::renamePhi(const IDominatorAnalysis &ida, BasicBlock &bb) {
  for (auto insn_iter = bb.begin(); insn_iter != bb.end();) {
    if (insn_iter->op == OP_Phi) {
      if (phiAllocaMap.count(insn_iter.base()) == 0)
        throw std::runtime_error("phi is not associated with an allocation");
      auto *alloc = phiAllocaMap.at(insn_iter.base());
      allocaLatest[&bb][alloc] = insn_iter.base();
    } else if (insn_iter->op == OP_Load) {
      auto *v = insn_iter->getInput()[0].from;
      if (v->is<Instruction *>() &&
          v->as<Instruction *>()->op == OP_Allocate) {
        auto *alloc = v->as<Instruction *>();
        Value *latest;
        if (containValue(&bb, alloc))
          latest = getValue(&bb, alloc);
        else
          throw std::runtime_error("undef here");
        insn_iter->replaceAllUsesWith(latest);
        insn_iter.release_and_increase(true);
        continue;
      }
    } else if (insn_iter->op == OP_Store) {
      auto *v = insn_iter->getInput()[0].from;
      if (v->is<Instruction *>() &&
          v->as<Instruction *>()->op == OP_Allocate) {
        auto *alloc = v->as<Instruction *>();
        auto *src = insn_iter->getInput()[1].from;
        assert(alloc != nullptr && alloc->op == OP_Allocate);
        allocaLatest[&bb][alloc] = src;
        insn_iter.release_and_increase(true);
        continue;
      }
    }
    ++insn_iter;
  }

  auto [_, idom] = ida.getIDominatorMap();
  auto range = idom.equal_range(&bb);
  for (auto iter = range.first; iter != range.second; iter++) {
    if (allocaLatest.count(&bb) != 0)
      allocaLatest[iter->second] = allocaLatest[&bb];
  }

  for (auto &e : bb.getSuccessor()) {
    auto *succ = e.to;
    renamePhiInSuccessorBasicBlock(*succ, bb);
  }
}

void PromoteMem2Reg::promoteMem2Reg(const IDominatorAnalysis &ida,
                                    IteratedDominanceFrontierAnalysis &idfa,
                                    Function &func) {
  if (func.refExternal())
    return;
  auto &alloca_bb = func.block.front();
  for (auto &alloca_insn : alloca_bb) {
    if (isPromotable(alloca_insn))
      insertPhiForAlloca(idfa, alloca_insn);
  }
  auto bfs = bfsTraversal<false>(&func);
  for (auto *bb : bfs) renamePhi(ida, *bb);
  clearVisited(&func);
}

void PromoteMem2Reg::operator()(IRHost &host) {
  IteratedDominanceFrontierAnalysis idfa;
  IDominatorAnalysis ida;
  idfa(host);
  ida(host);

  for (auto *func : host.getModule()->func) {
    if (func->refExternal())
      continue;
    promoteMem2Reg(ida, idfa, *func);
  }
}

Value *PromoteMem2Reg::getValue(BasicBlock *bb, Instruction *alloc) {
  return allocaLatest.at(bb).at(alloc);
}

bool PromoteMem2Reg::containValue(BasicBlock *bb, Instruction *alloc) {
  return allocaLatest.count(bb) != 0 && allocaLatest.at(bb).count(alloc) != 0;
}

bool PromoteMem2Reg::isPromotable(Instruction &alloca) {
  return alloca.op == OP_Allocate &&
         alloca.getInput()[0].from->as<SyOC::ConstantInteger *>()->value == 4;
}
} // namespace SyOC
