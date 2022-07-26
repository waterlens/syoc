#pragma once

/// \brief Macro Expansion Instruction Selection

#include "IR/IR.hpp"
#include "IR/ASM.hpp"

namespace SyOC {


class MEISel {
private:
  ARMv7a::MInstHost *machine;
  ARMv7a::MFunction *function;
  ARMv7a::MBasicBlock *basic_block;
private:
  /// Expand offset, call, br, memset0 to appropriate MInsts.
  bool expandInst(Instruction *I);
  bool selectRdRnOperand2(Instruction *I);
  bool selectRdRnRm(Instruction *I);
  bool selectRdRnImm(Instruction *I);
  bool selectLabel(Instruction *I);

public:
  MEISel() {}
  void operator()(IRHost &host);
  bool selectInstruction(Instruction *I);
  ARMv7a::Register CreateVirtualRegister(Value *V);

  /// Return a register of given IR Value;
  /// may create ldr/mov the load imm into a virtual register.
  ARMv7a::Register RegisterOrImm(Value *V);
  // ARMv7a::MInstruction *CreateMachineInst();
  ARMv7a::MFunction CreateISelFunction(Function *F);
};

} // end namespace SyOC