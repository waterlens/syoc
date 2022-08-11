
#include "PeepHole.hpp"
using namespace SyOC::ARMv7a;



void PeepHole::mergeCopy(MBasicBlock *MBB, MInstHost *Host) {
  for (auto I = MBB->insn.begin(), E = MBB->insn.end();
       I != E && I->next() != E.base(); ++I)
  {
    auto *NextI = I->next();
    if (NextI->op == Opcode::CPY && NextI->rb.id == I->ra.id
        && I->op != Opcode::STR) {
      I->ra.id = NextI->ra.id;
      work_list.push_back(NextI);
      ++I;
    }
    if (I->op == Opcode::MOV) {
      Shift SF = std::get<Shift>(I->rc.offset_or_else);
      if (SF.isPureImm() && NextI->rc.hasShift()) {
        Shift LaterSF = std::get<Shift>(NextI->rc.offset_or_else);
        if (LaterSF.reg.id == I->ra.id) {
          NextI->rc.offset_or_else = SF;
          work_list.push_back(I.base());
        }
      }
    }
  }
  for (auto *DeadI : work_list)
    DeadI->release(true);
}

void PeepHole::removeMemoryAccess(MBasicBlock *MBB, MInstHost *Host) {
  for (auto I = MBB->insn.begin(), E = MBB->insn.end();
       I != E && I->next() != E.base(); ++I)
  {
    auto *NextI = I->next();
    bool NeedRemove = false;
    // Store after Load.
    if (I->op == Opcode::STR && NextI->op == Opcode::LDR &&
        I->ra.id == NextI->ra.id)  {
      // remove ldr after str for same stack addr.
      if (I->rc.isStack() && NextI->rc.isStack()) {
       if (std::get<int>(I->rc.base) == std::get<int>(NextI->rc.base) &&
          std::get<int32_t>(I->rc.offset_or_else) == std::get<int>(NextI->rc.offset_or_else))
         NeedRemove = true; break;
      }
      // remove ldr after str same stack addr.
      if (I->rc.isPointerOrGlobal() && NextI->rc.isPointerOrGlobal()) {
        if (std::get<Register>(I->rc.base).id == std::get<Register>(NextI->rc.base).id &&
            std::get<int32_t>(I->rc.offset_or_else) == std::get<int>(NextI->rc.offset_or_else))
          NeedRemove = true; break;
      }
    }
    // Load After Store.
    if (I->op == Opcode::LDR && NextI->op == Opcode::STR &&
        I->ra.id == NextI->ra.id)  {
      // remove str after ldr for same stack addr.
      if (I->rc.isStack() && NextI->rc.isStack()) {
        if (std::get<int>(I->rc.base) == std::get<int>(NextI->rc.base) &&
            std::get<int32_t>(I->rc.offset_or_else) == std::get<int>(NextI->rc.offset_or_else))
          NeedRemove = true; break;
      }
      // remove str after ldr same stack addr.
      if (I->rc.isPointerOrGlobal() && NextI->rc.isPointerOrGlobal()) {
        if (std::get<Register>(I->rc.base).id == std::get<Register>(NextI->rc.base).id &&
            std::get<int32_t>(I->rc.offset_or_else) == std::get<int>(NextI->rc.offset_or_else))
          NeedRemove = true; break;
      }
    }
    if (NeedRemove) NextI->release(true);
  }
}

void PeepHole::operator()(MInstHost &host) {
  host.clearInsertPoint();
  for (auto *MF : host.root->function) {
    if (MF->refExternal)
      continue;
    for (auto &MBB : MF->block) {
      work_list.clear();
      /// @attention
      /// movw r0, xxx
      /// movwt r0, xxx
      /// cpy  xx, r0
      // mergeCopy(&MBB, &host);
      removeMemoryAccess(&MBB, &host);
    }
  }
}