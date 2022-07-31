#pragma once

#include "IR/IR.hpp"
#include "Util/List.hpp"

#include <cassert>
#include <utility>
#include <variant>
#include <vector>
#include <map>

namespace SyOC::ARMv7a {

// 16 int + 32 float
static const unsigned int RegCount = 48;

static const char *RegisterNames[] = {
#define TargetIntegralRegister(x, y, z) #x,
#define TargetFloatRegister(x, y, z) #x,
#include "Common/TargetInfo.def"
};


struct Register {
  enum IntRegs {
#define FirstIntReg(x) INTEGER_REG_BEGIN = x,
#define TargetIntegralRegister(x, y, z) x = z,
#define LastIntReg(x) INTEGER_REG_END = x,
#include "Common/TargetInfo.def"
  };
  enum VFPRegs {
#define FirstVFPReg(x) VFP_REG_BEGIN = x,
#define TargetFloatRegister(x, y, z) x = z,
#define LastVFPReg(x) VFP_REG_END = x,
#include "Common/TargetInfo.def"
  };
  int id = -1;

  inline bool isInvalid() const { return id == -1; }
  inline bool isVirtual() const { return !(isInteger() || isFloat()) && id > 0; }
  inline bool isInteger() const { return id >= INTEGER_REG_BEGIN && id <= INTEGER_REG_END; }
  inline bool isFloat() const { return id >= VFP_REG_BEGIN && id <= VFP_REG_END; }
  static const char *getName(const Register &r) {
    if (r.isInteger() || r.isFloat()) return RegisterNames[r.id];
    return nullptr;
  }
};

struct RegisterList {
  uint32_t ls = 0; // vmov, vpush requires 32 bit.
  // std::bitset<RegCount> ls;
};

struct FrameObject {
  size_t index; //  size of the current frame object in function.
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
  int32_t imm; // pure Imm or shift bits
  Register reg;

  static Shift GetDefaultShift(Register r) {
    return Shift {Type::SF_None, 0, r};
  }
  static Shift GetImm(int32_t Imm) {
    return Shift {Type::SF_None, Imm, -1};
  }
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
  std::variant<Register, FrameObject> base; // global / stack object
  std::variant<int32_t, Register, Shift, RegisterList, MBasicBlock *, MFunction *>
    offset_or_else;

  bool isPointerOrGlobal() const { return std::holds_alternative<Register>(base); }
  bool isStack() const { return std::holds_alternative<FrameObject>(base); }
};

struct MInstruction : public ListNode<MInstruction> {
#define assert_op_format(fmt) assert(get_op_format(op) == Format::fmt)
  MBasicBlock *parent = nullptr;
  Opcode op = Opcode::NOP;
  Condition cond = Condition::CT_Any;
  Register ra = Register{-1};             // Rd, RdLo, Rn
  Register rb = Register{-1};             // Rm, Rn, RdHi
  Address rc = Address{Register{-1}, -1}; // Rs, Rm, Rn, imm, Operand2, label, function (esp. only declaration)


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

  // pointer or global memory access with definite register
  static MInstruction *RdRnImm(Opcode op, Register rd, Register rn, int imm,
                               Condition cond = Condition::CT_Any) {
    assert_op_format(IF_RdRnImm);
    auto p = create();
    p->op = op;
    p->ra = rd;
    p->rc.base = rn;
    p->rc.offset_or_else = imm;
    p->cond = cond;
    return p;
  }

  // stack memory access with intermediate FrameObject representation,
  // will be lowered to [sp, #offset] afterward.
  static MInstruction *RdRnImm(Opcode op, Register rd, FrameObject rn, int32_t imm,
                               Condition cond = Condition::CT_Any) {
    assert_op_format(IF_RdRnImm);
    auto p = create();
    p->op = op;
    p->ra = rd;
    p->rc.base = rn;
    p->rc.offset_or_else = imm;
    p->cond = cond;
    return p;
  }

  static MInstruction *RdImm(Opcode op, Register rd, int32_t imm,
                             Condition cond = Condition::CT_Any) {
    assert_op_format(IF_RdImm);
    auto p = create();
    p->op = op;
    p->ra = rd;
    p->rc.offset_or_else = imm;
    p->cond = cond;
    return p;
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

  static MInstruction *RdRmRnRa(Opcode op, Register rd, Register rm,
                                Register rs, Register rn,
                                Condition cond = Condition::CT_Any) {
    assert_op_format(IF_RdRmRnRa);
    auto &p = *create();
    p.op = op;
    p.ra = rd;
    p.rb = rm;
    p.rc.base = rs;
    p.rc.offset_or_else = rn;
    p.cond = cond;
    return &p;
  }

  static MInstruction *RdLoRdHiRnRm(Opcode op, Register lo, Register hi,
                                    Register rm, Register rs,
                                    Condition cond = Condition::CT_Any) {
    assert_op_format(IF_RdLoRdHiRnRm);
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
  std::vector<size_t> frame; // stack frame sizes
  std::string name;

  std::map<Value *, Register> value_map; // IR Value to register id.
  std::map<Value *, FrameObject> frame_info;

  MBasicBlock *CreateBasicBlock();
  FrameObject *GetStackObject(Value *V);
  FrameObject *CreateStackObject(Value *V, size_t size);
  Register LookUpRegister(Value *);
  Register LookUpFrame(Value *);
};

struct MModule {
  std::vector<MFunction> function;
  std::vector<GlobalVariable> global;
};

// \brief Hosting Lowered IR and MInsts after register allocation
struct MInstHost {
  MModule *root;
  size_t label_cnt;

  // @TODO: Stack Frame Info, MInst Builder Helper Info, Liveness Analysis

};

} // namespace SyOC::ARMv7a