#include "CFGAnalysis.hpp"

namespace SyOC {

void CFGAnalysis::operator()(IRHost &host) {
  assignIdentity(host);
  for (auto *func : host.getModule()->func) {
    for (auto &bb : func->block) bb.getPredecessor().clear();
    for (auto &bb : func->block) {
      if (bb.getInstruction().empty())
        throw std::runtime_error("empty basic block");
      auto &last = bb.getInstruction().back();
      if (!last.isControlInstruction())
        throw std::runtime_error(
          "not a control instruction at the end of basic block");
      if (last.op == OP_Return) {
        // do nothing
      } else if (last.op == OP_Branch) {
        auto &a1 = last.getInput()[1];
        auto &a2 = last.getInput()[2];

        assert(a1.from->is<BasicBlock *>() && a2.from->is<BasicBlock *>());
        auto *bb1 = a1.from->as<BasicBlock *>();
        auto *bb2 = a2.from->as<BasicBlock *>();

        bb1->addPredecessor(&bb);
        bb2->addPredecessor(&bb);
      } else if (last.op == OP_Jump) {
        auto &a1 = last.getInput()[0];

        assert(a1.from->is<BasicBlock *>());
        auto *bb1 = a1.from->as<BasicBlock *>();

        bb1->addPredecessor(&bb);
      }
    }
  }
}
} // namespace SyOC