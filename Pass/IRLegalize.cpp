
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

void IRLegalize::insertMemset0(Function *F, IRHost &Host) {
  for (auto &BB : F->block) {
    for (auto I = BB.begin(), E = BB.end();
         I != E; ++I)
    {
      if (I->op == OP_Allocate) {
        uint32_t Size = I->getOperand(0)->as<ConstantInteger *>()->value;
        if (Size <= 4)
          continue;
        Instruction *memset = Instruction::create(OP_Call, PredefinedType::Void,
                    {memset0, I.base(), ConstantInteger::create(0), I->getOperand(0)});
        I->insert_after(memset);
      }
    }
  }
}

void IRLegalize::operator()(IRHost &host) {
  aeabi_idiv = *std::find_if(host.root->func.begin(), host.root->func.end(),
                            [](Function *F) { return F->name == "__aeabi_idiv"; });
  aeabi_idivmod = *std::find_if(host.root->func.begin(), host.root->func.end(),
                           [](Function *F) { return F->name == "__aeabi_idivmod"; });
  // memset0
  Type Void = PredefinedType::Void;
  Type VoidP = PredefinedType::VoidPtr;
  Type IntT = PredefinedType::Int32;
  memset0 = Function::create(Void, "memset", nullptr);
  memset0->refExternal() = true;
  Argument *S = Argument::create(VoidP, "__s", memset0);
  Argument *C = Argument::create(IntT, "__c", memset0);
  Argument *N = Argument::create(IntT, "__n", memset0);
  host.root->func.insert(host.root->func.begin(), memset0);

  for (auto *F : host.root->func) {
    virtualExtension(F, host);
    insertMemset0(F, host);
    for (auto *dead_inst : work_list)
      dead_inst->release(true);
    work_list.clear();
  }
}