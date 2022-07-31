#include "InstCombine.hpp"

namespace SyOC {

static OpType conjugateComparison(OpType Op) {
  switch (Op) {
  case OP_Ge: return OP_Lt;
  case OP_Gt: return OP_Le;
  case OP_Le: return OP_Gt;
  case OP_Lt: return OP_Ge;
  case OP_Eq: return OP_Ne;
  case OP_Ne: return OP_Eq;
  }
  return OP_None;
}

void InstCombine::comparisonSimplify(IRHost &host) {
  // eliminate redundant not
  for (Function *F : host.getModule()->func) {
    for (BasicBlock &BB : F->block) {
      for (auto I = BB.begin(), E = BB.end();
           I != E; ++I) {
        if (I->op == OP_Lnot) {
            Instruction *Expr = I->getOperand(0)->as<Instruction *>();
            assert(Expr);
            // If lnot a comparison like:
            //  %8 = lt %7, 10 -> Expr
            //  %9 = lnot %8 -> I
            //  br %9 label1, label2
            // We replace %9 with a ge %7, 10,
            // %9 is expected to be eliminated in later passes.
            if (Expr->isCompareInst()) {
              // BB is the father basic block.
              OpType neg_op = conjugateComparison(Expr->op);
              assert(neg_op != OP_None);
              auto deduced_inst = host.createInstruction(neg_op, I->type,
                                     {Expr->getOperand(0), Expr->getOperand(1)},
                                     &BB);
              I->replaceAllUsesWith(deduced_inst);
            }
            if (Expr->op == OP_Lnot || Expr->op == OP_Neg) {
              I->replaceAllUsesWith(Expr->getOperand(0));
            }
        }
      }
    }
  }
}

} // end namespace SyOC