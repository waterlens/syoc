#pragma once

#include "IR/IR.hpp"
#include "Util/List.hpp"

#include <cassert>
#include <utility>
#include <variant>
#include <vector>

namespace SyOC::ARMv7a {

struct Register {
  int id = -1;
};

struct RegisterList {
  short ls = 0;
};

enum class Opcode : unsigned char {
#define ARMv7aOpcodeDefine(x, y, z) x,
#include "Common/Common.def"
#undef ARMv7aOpcodeDefine
};

enum class Format {
#define ARMv7aInsnFormatDefine(x) x,
#include "Common/Common.def"
#undef ARMv7aInsnFormatDefine
};

inline Format get_op_format(Opcode op) {
#define ARMv7aOpcodeDefine(x, y, z)                                            \
  if (op == Opcode::x)                                                         \
    return Format::z;
#include "Common/Common.def"
#undef ARMv7aOpcodeDefine
  return Format::IF_Other;
}

struct Shift {
  enum class Type : unsigned char {
#define ShiftTypeDefine(x, y) x,
#include "Common/Common.def"
#undef ShiftTypeDefine
  } type;
  signed char imm;
  Register reg;
};

enum class Condition : unsigned char {
#define ConditionTypeDefine(x, v) x = (v),
#include "Common/Common.def"
#undef ConditionTypeDefine
};

struct MBasicBlock;
struct MInstruction;
struct MFunction;
struct MModule;

struct Address {
  Register base;
  std::variant<int, Register, Shift, RegisterList, MBasicBlock *>
    offset_or_else;
};

struct MInstruction : public ListNode<MInstruction> {
#define assert_op_format(fmt) assert(get_op_format(op) == Format::fmt)
  MBasicBlock *parent = nullptr;
  Opcode op = Opcode::NOP;
  Condition cond = Condition::CT_Any;
  Register ra = Register{-1};             // Rd, RdLo, Rn
  Register rb = Register{-1};             // Rm, Rn, RdHi
  Address rc = Address{Register{-1}, -1}; // Rs, Rm, Rn, imm, Operand2, label
  static MInstruction *create() { return new MInstruction; }
  static MInstruction *RdRnRm(Opcode op, Register rd, Register rn, Register rm,
                              Condition cond = Condition::CT_Any) {
    assert_op_format(IF_RdRnRm);
    auto &p = *create();
    p.op = op;
    p.ra = rd;
    p.rb = rn;
    p.rc.base = rm;
    p.cond = cond;
    return &p;
  }

  static MInstruction *RdRm(Opcode op, Register rd, Register rm,
                            Condition cond = Condition::CT_Any) {
    assert_op_format(IF_RdRm);
    auto &p = *create();
    p.op = op;
    p.ra = rd;
    p.rb = rm;
    p.cond = cond;
    return &p;
  }

  static MInstruction *RdRmRs(Opcode op, Register rd, Register rm, Register rs,
                              Condition cond = Condition::CT_Any) {
    assert_op_format(IF_RdRmRs);
    auto &p = *create();
    p.op = op;
    p.ra = rd;
    p.rb = rm;
    p.rc.base = rs;
    p.cond = cond;
    return &p;
  }

  static MInstruction *RdRmRsRn(Opcode op, Register rd, Register rm,
                                Register rs, Register rn,
                                Condition cond = Condition::CT_Any) {
    assert_op_format(IF_RdRmRsRn);
    auto &p = *create();
    p.op = op;
    p.ra = rd;
    p.rb = rm;
    p.rc.base = rs;
    p.rc.offset_or_else = rn;
    p.cond = cond;
    return &p;
  }

  static MInstruction *RdLoRdHiRmRs(Opcode op, Register lo, Register hi,
                                    Register rm, Register rs,
                                    Condition cond = Condition::CT_Any) {
    assert_op_format(IF_RdLoRdHiRmRs);
    auto &p = *create();
    p.op = op;
    p.ra = lo;
    p.rb = hi;
    p.rc.base = rm;
    p.rc.offset_or_else = rs;
    p.cond = cond;
    return &p;
  }

  static MInstruction *RdRnOperand2(Opcode op, Register rd, Register rm,
                                    Shift sf,
                                    Condition cond = Condition::CT_Any) {
    assert_op_format(IF_RdRnOperand2);
    auto &p = *create();
    p.op = op;
    p.ra = rd;
    p.rb = rm;
    p.rc.offset_or_else = sf;
    p.cond = cond;
    return &p;
  }

  static MInstruction *RdOperand2(Opcode op, Register rd, Shift sf,
                                  Condition cond = Condition::CT_Any) {
    assert_op_format(IF_RdOperand2);
    auto &p = *create();
    p.op = op;
    p.ra = rd;
    p.rc.offset_or_else = sf;
    p.cond = cond;
    return &p;
  }

  static MInstruction *RnOperand2(Opcode op, Register rn, Shift sf,
                                  Condition cond = Condition::CT_Any) {
    assert_op_format(IF_RnOperand2);
    auto &p = *create();
    p.op = op;
    p.rb = rn;
    p.rc.offset_or_else = sf;
    p.cond = cond;
    return &p;
  }

  static MInstruction *Label(Opcode op, MBasicBlock *label,
                             Condition cond = Condition::CT_Any) {
    assert_op_format(IF_Label);
    auto &p = *create();
    p.op = op;
    p.rc.offset_or_else = label;
    p.cond = cond;
    return &p;
  }
#undef assert_op_format
};

struct MBasicBlock : public ListNode<MBasicBlock> {
  List<Instruction> insn;
  std::vector<MBasicBlock *> succ;
  std::vector<MBasicBlock *> pred;
};

struct MFunction {
  List<MBasicBlock> block;
  std::string name;
};

} // namespace SyOC::ARMv7a