
#include "IRLegalize.hpp"
using namespace SyOC;

void IRLegalize::virtualExtension(Function *F, IRHost &Host) {
  for (auto &BB : F->block) {
    for (auto I = BB.begin(), E = BB.end();
         I != E; ++I)
    {
      if (I->op == OP_Div) {
        Instruction *EABI_I = Instruction::create(OP_Call, I->type,
                              {aeabi_idiv, I->getOperand(0), I->getOperand(1)});
        I->insert_before(EABI_I);
        I->replaceAllUsesWith(EABI_I);
        work_list.push_back(I.base());
      }
      if (I->op == OP_Mod) {
        Instruction *EABI_I = Instruction::create(OP_Call, I->type,
                              {aeabi_idivmod, I->getOperand(0), I->getOperand(1)});
        I->insert_before(EABI_I);
        I->replaceAllUsesWith(EABI_I);
        work_list.push_back(I.base());
      }
    }
  }
}

void IRLegalize::operator()(IRHost &host) {
  aeabi_idiv = *std::find_if(host.root->func.begin(), host.root->func.end(),
                            [](Function *F) { return F->name == "__aeabi_idiv"; });
  aeabi_idivmod = *std::find_if(host.root->func.begin(), host.root->func.end(),
                           [](Function *F) { return F->name == "__aeabi_idivmod"; });
  for (auto *F : host.root->func) {
    virtualExtension(F, host);
    for (auto *dead_inst : work_list)
      dead_inst->release(true);
    work_list.clear();
  }
}