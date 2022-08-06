
#include "ConstantFolding.hpp"

using namespace SyOC;


void ConstantFolding::foldConstant(IRHost &host) {
  for (auto *func : host.root->func) {
    for (auto &BB : func->block) {
      for (auto I = BB.begin(), E = BB.end(); I != E;
           ++I)
      {
        // Algebraic identities & Constant folding.
        if (I->getNumOperands() == 1) {
          Value *Op1 = I->getOperand(0);
          if (Op1->is<ConstantInteger>()) {
            int64_t IntOp1 = static_cast<int64_t>(Op1->as<ConstantInteger *>()->value);
            ConstantInteger *FoldedInt;
            switch (I->op) {
            case OP_Neg: FoldedInt = ConstantInteger::create(-IntOp1);
              break;
            case OP_Lnot: FoldedInt = ConstantInteger::create(IntOp1 == 0);
              break;
            default:
              continue;
            }
            I->replaceAllUsesWith(FoldedInt);
            work_list.push_back(I.base());
          }
        }
        if (I->getNumOperands() == 2) {
            Value *Op1 = I->getOperand(0);
            Value *Op2 = I->getOperand(1);
            if (Op1->is<ConstantInteger>() && Op2->is<ConstantInteger>()) {
              int64_t IntOp1 = static_cast<int64_t>(Op1->as<ConstantInteger *>()->value);
              int64_t IntOp2 = static_cast<int64_t>(Op2->as<ConstantInteger *>()->value);
              ConstantInteger *FoldedInt;
              switch (I->op) {
              case OP_Add : FoldedInt = ConstantInteger::create(
                  static_cast<uint64_t>(IntOp1 + IntOp2)); break;
              case OP_Sub : FoldedInt = ConstantInteger::create(
                  static_cast<uint64_t>(IntOp1 - IntOp2)); break;
              case OP_Mul : FoldedInt = ConstantInteger::create(
                  static_cast<uint64_t>(IntOp1 * IntOp2)); break;
              case OP_Div: FoldedInt = ConstantInteger::create(
                  static_cast<uint64_t>(IntOp1 / IntOp2)); break;
              case OP_Mod: FoldedInt = ConstantInteger::create(
                  static_cast<uint64_t>(IntOp1 % IntOp2)); break;
              case OP_Lt: FoldedInt = ConstantInteger::create(IntOp1 < IntOp2);
                break;
              case OP_Le: FoldedInt = ConstantInteger::create(IntOp1 <= IntOp2);
                break;
              case OP_Gt: FoldedInt = ConstantInteger::create(IntOp1 > IntOp2);
                break;
              case OP_Ge: FoldedInt = ConstantInteger::create(IntOp1 >= IntOp2);
                break;
              case OP_Ne: FoldedInt = ConstantInteger::create(IntOp1 != IntOp2);
                break;
              case OP_Eq: FoldedInt = ConstantInteger::create(IntOp1 == IntOp2);
                break;
              default:
                continue;
              }
              I->replaceAllUsesWith(FoldedInt);
              work_list.push_back(I.base());
            }
        }
      }
    }
  }
  deadCodeElimination();
}

void ConstantFolding::foldIdentity(IRHost &host) {
  for (auto *func : host.root->func) {
    for (auto &BB : func->block) {
      for (auto I = BB.begin(), E = BB.end(); I != E;
           ++I)
      {
        if (I->getNumOperands() == 2) {
          bool isDead = false;
          if (auto *Op2 = I->getOperand(1)->as<ConstantInteger *>()) {
            int64_t IntOp2 = static_cast<int64_t>(Op2->value);
            switch (I->op) {
            case OP_Add:
              if (IntOp2 == 0) {
                I->replaceAllUsesWith(I->getOperand(0));
                isDead = true;
              }
              break;
            case OP_Sub:
              if (IntOp2 == 0) {
                I->replaceAllUsesWith(I->getOperand(0));
                isDead = true;
              }
              break;
            case OP_Mul:
              if (IntOp2 == 0) {
                I->replaceAllUsesWith(ConstantInteger::create(0));
                isDead = true;
              }
              if (IntOp2 == 1) {
                I->replaceAllUsesWith(I->getOperand(0));
                isDead = true;
              }
              break ;
            case OP_Div:
              if (IntOp2 == 1) {
                I->replaceAllUsesWith(I->getOperand(0));
                isDead = true;
              }
              break;
            case OP_Mod:
              if (IntOp2 == 1) {
                I->replaceAllUsesWith(ConstantInteger::create(0));
                isDead = true;
              }
              break;
            default:
              continue;
            }
            if (isDead) work_list.push_back(I.base());
          }
        }
      }
    }
  }
  deadCodeElimination();
}

void ConstantFolding::deadCodeElimination() {
  for (auto inst_iter : work_list)
    inst_iter->release(true);
  work_list.clear();
}

void ConstantFolding::operator()(IRHost &host) {
  foldConstant(host);
  foldIdentity(host);
}