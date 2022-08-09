
#include "MachineDCE.hpp"
using namespace SyOC::ARMv7a;

void MachineDCE::removeDeadMov(MInstHost &host) {
  for (auto *mfunc : host.root->function) {
    if (mfunc->refExternal)
      continue;
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
}



void MachineDCE::operator()(MInstHost &host) {
  removeDeadMov(host);
  for (auto *dead_inst_iter : work_list)
    dead_inst_iter->release(true);
  work_list.clear();
}