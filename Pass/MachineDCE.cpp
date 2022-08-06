
#include "MachineDCE.hpp"
using namespace SyOC::ARMv7a;

void MachineDCE::removeDeadMov(MInstHost &host) {
  for (auto *mfunc : host.root->function) {
    for (auto &mbb : mfunc->block) {
      for (auto &minst : mbb.insn) {
        if (minst.op == Opcode::MOV || minst.op == Opcode::CPY) {
          if (minst.ra.id == minst.rb.id)
            work_list.push_back(&minst);
        }
      }
    }
  }
}


void MachineDCE::operator()(MInstHost &host) {
  removeDeadMov(host);
  for (auto *dead_minst : work_list)
    dead_minst->release(true);
  work_list.clear();
}