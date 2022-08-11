
#include "MLivenessAnalysis.hpp"
using namespace SyOC::ARMv7a;

void MLivenessAnalysis::operator()(MFunction *MF) {
  LiveIn.clear();
  LiveOut.clear();
  std::vector<Register *> reg_use;
  std::vector<Register *> reg_def;
  // calculate MInstrution Liveness.
  for (auto &MBB : MF->block) {
    for (auto MI = MBB.insn.end(), MIE = MBB.insn.begin();
         MI-- != MIE;)
    {
      LiveIn[MI->id] = {};
      LiveOut[MI->id] = {};
      getUse(MI.base(), reg_use);
      getDef(MI.base(), reg_def);
      auto &IOut = getLiveOuts(MI->id);
      if (MI->next() != MBB.insn.end().base()) {
        auto &NextIIn = getLiveIns(MI->next()->id);
        IOut = NextIIn;
      }
      auto &IIn = getLiveIns(MI->id);
      for (auto *reg : reg_def) {
        IIn.erase(reg->id);
      }
      for (auto *reg : reg_use) {
        IIn.insert(reg->id);
      }
    }
  }

}