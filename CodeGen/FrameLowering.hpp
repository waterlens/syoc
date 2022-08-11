#pragma once

#include "IR/ASM.hpp"

namespace SyOC {
namespace ARMv7a {

class FrameLowering {
private:
  std::vector<MInstruction *> work_list;
  size_t final_stack_size;
  static constexpr size_t stack_align = 8;

  void emitPrologue(MFunction *, MInstHost *);
  void emitEpilogue(MFunction *, MInstHost *);
  void lowering(MFunction *, MInstHost *);
  void legalizeOffset(MFunction *, MInstHost *);
  void deadCodeElimination();
public:
  static std::string_view getName() { return "Frame Lowering";}
  void operator ()(MInstHost &host);
};

} // end namespace ARMv7a
} // end namespace SyOC
