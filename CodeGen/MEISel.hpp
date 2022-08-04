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

  bool selectMemoryAccess(Instruction *I);
  bool selectComparison(Instruction *I);
  bool copyPhiNodesRegs(BasicBlock *SyOCBB);
  bool selectRdRnOperand2(Instruction *I);
  bool selectRdRnRm(Instruction *I);
  bool selectRdRnImm(Instruction *I);
#define OpcodeDefine(type, name) \
  bool select##type(Instruction *I);
#include "Common/Common.def"


  ARMv7a::Register CreateVirtualRegister(Value *V); // for definitions.
  /// Return a register of given IR Value;
  /// may create ldr/mov the load imm into a virtual register.
  ARMv7a::Register RegisterOrImm(Value *V); // for operands.

public:
  MEISel() = default;
  void operator()(IRHost *host, ARMv7a::MInstHost *&mhost);
  [[nodiscard]] static std::string_view getName() { return "Macro Expansion ISel";}
  bool selectInstruction(Instruction *I);
  void setInsertPoint(ARMv7a::MBasicBlock *mbb) { basic_block = mbb; }

};

} // end namespace SyOC