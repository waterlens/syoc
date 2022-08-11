
#include "MachineDCE.hpp"
using namespace SyOC::ARMv7a;

void MachineDCE::removeDeadMov(MFunction *mfunc, MInstHost &host) {
  for (auto &mbb : mfunc->block) {
    for (auto inst_iter = mbb.insn.begin(), inst_end = mbb.insn.end();
         inst_iter != inst_end; ++inst_iter)
    {
      // @TODO: CPY and MOV has different Format.
      if (inst_iter->op == Opcode::CPY) {
        if (inst_iter->ra.id == inst_iter->rb.id)
          work_list.push_back(inst_iter.base());
      }
      if (inst_iter->op == Opcode::MOV) {
        if (inst_iter->ra.id == std::get<Shift>(inst_iter->rc.offset_or_else).reg.id)
          work_list.push_back(inst_iter.base());
      }
      if (inst_iter->op == Opcode::CLEARUSE)
        work_list.push_back(inst_iter.base());
    }
  }
}

void MachineDCE::removeDeadArithmetic(MFunction *MF, MInstHost &MHost) {
  for (auto &MBB : MF->block) {
    for (auto inst_iter = MBB.insn.begin(), inst_end = MBB.insn.end();
         inst_iter != inst_end; ++inst_iter) {
      if (inst_iter->op == Opcode::ADD) {
        Register Rd = inst_iter->ra;
        Register Rn = inst_iter->rb;
        Shift SF = std::get<Shift>(inst_iter->rc.offset_or_else);
        if (SF.isPureImm() && SF.imm == 0 && Rd.id == Rn.id)
          work_list.push_back(inst_iter.base());
      }
    }
  }
}

void MachineDCE::operator()(MInstHost &host) {
  for (auto *mfunc : host.root->function) {
    if (mfunc->refExternal)
      continue;
    removeDeadMov(mfunc, host);
    removeDeadArithmetic(mfunc, host);
    for (auto *dead_inst_iter : work_list)
      dead_inst_iter->release(true);
    work_list.clear();
  }
}