#include "InstCombine.hpp"

namespace SyOC {

// negate the comparison.
static OpType conjugateComparison(OpType Op) {
  switch (Op) {
  case OP_Ge: return OP_Lt;
  case OP_Gt: return OP_Le;
  case OP_Le: return OP_Gt;
  case OP_Lt: return OP_Ge;
  case OP_Eq: return OP_Ne;
  case OP_Ne: return OP_Eq;
  default: return OP_None;
  }
}

// switch operands.
static OpType reverseComparison(OpType Op) {
  switch (Op) {
  case OP_Ge: return OP_Le;
  case OP_Gt: return OP_Lt;
  case OP_Le: return OP_Ge;
  case OP_Lt: return OP_Gt;
  case OP_Eq: return OP_Eq;
  case OP_Ne: return OP_Ne;
  default: return OP_None;
  }
}

void InstCombine::comparisonSimplify(IRHost &host) {
  // eliminate redundant not
  for (Function *F : host.getModule()->func) {
    for (BasicBlock &BB : F->block) {
      for (auto I = BB.begin(), E = BB.end(); I != E; ++I) {
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
              auto deduced_inst = Instruction::create(neg_op, I->type,
                                  {Expr->getOperand(0), Expr->getOperand(1)});
              I->insert_before(deduced_inst);
              I->replaceAllUsesWith(deduced_inst);
              work_list.push_back(I);
            }
            if (Expr->op == OP_Lnot || Expr->op == OP_Neg) {
              I->replaceAllUsesWith(Expr->getOperand(0));
              work_list.push_back(I);
            }
        }
      }
    }
  }
}

void InstCombine::switchOperands(IRHost &host) {
  for (Function *F : host.getModule()->func) {
    for (BasicBlock &BB : F->block) {
      for (auto I = BB.begin(), E = BB.end(); I != E; ++I) {
        if (I->op == OP_Add || I->op == OP_Sub || I->op == OP_Mul) {
          // We assume after Constant-Folding, Binary Operators
          // has at most 1 constant operand, this pass guarantees
          // the constant operand is the second operand.
          if (I->getOperand(0)->is<ConstantInteger>() &&
              !I->getOperand(1)->is<ConstantInteger>())
            std::swap(I->getInput(0), I->getInput(1));
        }
        if (I->isCompareInst()) {
          if (I->getOperand(0)->is<ConstantInteger>() &&
              !I->getOperand(1)->is<ConstantInteger>())
          {
            OpType rev_op = reverseComparison(I->op);
            assert(rev_op != OP_None);
            auto deduced_inst = Instruction::create(rev_op, I->type,
                                                    {I->getOperand(1), I->getOperand(0)});
            I->insert_before(deduced_inst);
            I->replaceAllUsesWith(deduced_inst);
            work_list.push_back(I);
          }
        }
      }
    }
  }
}

void InstCombine::mergeOffset(IRHost &host) {
  for (Function *F : host.getModule()->func) {
    for (BasicBlock &BB : F->block) {
      for (auto I = BB.begin(), E = BB.end(); I != E; ++I) {
        if (I->op == OP_Offset) {
          auto *Addr = I->getOperand(0);
          if (auto BaseOff = Addr->as<Instruction *>()) {
            if (BaseOff->op != OP_Offset) continue;
            std::vector<Value *> Inputs;
            size_t BaseNum = BaseOff->getNumOperands();
            size_t INum = I->getNumOperands();
            // a single 1 dimension offset.
            assert(INum == 4);
            // exclude width of base offset.
            for (size_t i = 0; i < BaseNum - 1; ++i)
              Inputs.push_back(BaseOff->getOperand(i));
            /// @attention This feature is a ad hoc targeting
            /// current offset method.
            uint64_t BaseWidth = BaseOff->getOperand(BaseNum - 1)
                                   ->as<ConstantInteger *>()->value;
            uint64_t IWidth = I->getOperand(INum - 1)
                                  ->as<ConstantInteger *>()->value;
            uint64_t I1DSize = I->getOperand(1)
                                  ->as<ConstantInteger *>()->value;
            assert(BaseWidth == IWidth * I1DSize);
            for (size_t i = 1; i < INum; ++i)
              Inputs.push_back(I->getOperand(i));
            /// @attention May have future casting problem
            auto *NewOff = Instruction::create(OP_Offset, I->type, Inputs);
            I->insert_before(NewOff);
            I->replaceAllUsesWith(NewOff);
            if (I->hasNoEdge()) work_list.push_back(I);
          }
        }
      }
    }
  }
}

void InstCombine::deadCodeElimination(IRHost &host) {
  for (auto inst_iter : work_list)
    inst_iter.release(true);
  work_list.clear();
}

} // end namespace SyOC