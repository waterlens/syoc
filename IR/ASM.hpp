#pragma once

#include "IR/IR.hpp"
#include "Util/List.hpp"
#include <variant>

namespace SyOC::ARMv7a {

struct Register {
  unsigned id = 0;
};

struct RegisterList {
  short ls = 0;
};

struct Shift {
  enum class Type {
#define ShiftTypeDefine(x, y) x,
#include "Common/Common.def"
#undef ShiftTypeDefine
  } type;
  int imm;
  Register reg;
};

enum class Condition {
#define ConditionTypeDefine(x, v) x = (v),
#include "Common/Common.def"
#undef ConditionTypeDefine
};

struct Address {
  Register base;
  std::variant<unsigned, Register, Shift> offset;
};

struct MBasicBlock;
struct MInstruction;
struct MFunction;
struct MModule;

struct MInstruction : public ListNode<MInstruction> {
};

struct MBasicBlock : public ListNode<MBasicBlock> {};

} // namespace SyOC::ARMv7a