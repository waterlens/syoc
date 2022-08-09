#pragma once

#include "IR/IR.hpp"
#include "Util/List.hpp"

#include <cassert>
#include <utility>
#include <variant>
#include <vector>
#include <map>

namespace SyOC::ARMv7a {



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
  enum StatusRegs {
#define FirstStatusReg(x) STATUS_REG_BEGIN = x,
#define TargetStatusRegister(x, y, z) x = z,
#define LastStatusReg(x) STATUS_REG_END = x,
#include "Common/TargetInfo.def"
  };
  enum VirtualRegs {
#define FirstVirtualReg(x) VREG_BEGIN = x,
#include "Common/TargetInfo.def"
  };
  enum Type {
    Int,
    Float,
  };
  int id = -1;
  Type type = Int;

  inline bool isInvalid() const { return id == -1; }
  inline bool isVirtual() const { return !(isInteger() || isFloat() || isStatus()) && id > 0; }
  inline bool isInteger() const { return id >= INTEGER_REG_BEGIN && id <= INTEGER_REG_END; }
  inline bool isFloat() const { return id >= VFP_REG_BEGIN && id <= VFP_REG_END; }
  inline bool isStatus() const { return id >= STATUS_REG_BEGIN && id <= STATUS_REG_END; }
};

struct RegisterList {
  typedef uint32_t RegVector;
  static const int RegCount = 32;
  RegVector ls = 0; // vmov, vpush requires 32 bit.
  // std::bitset<RegCount> ls;
};

struct FrameObject {
  int32_t Offset;
  size_t Size;
  const Instruction* Alloca;
  bool isSpill;
  FrameObject(int32_t Offset, size_t Size,
              Instruction *Alloca = nullptr, bool isSpill = false) :
    Offset(Offset), Size(Size), Alloca(Alloca), isSpill(isSpill) { }
};

// llvm MachineFrameInfo.
struct CalleeSaved {
  Register Reg;
  union {
    int FrameIdx;
    unsigned DstReg;
  } Locate;
  bool SpilledToReg = false;
};

// 4 byte literal pool
struct LiteralPool {
  int id;
  uint32_t Literal;
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
  Register reg;
  int32_t imm; // pure Imm or shift bits

  static Shift GetDefaultShift(Register r) {
    return Shift {Type::SF_None, r, 0};
  }
  static Shift GetImm(int32_t Imm) {
    return Shift {Type::SF_None, Register{-1}, Imm};
  }


  bool isPureReg() const { return reg.id != -1 && imm == 0;}
  bool isPureImm() const { return reg.id == -1; }
};

enum class Condition : unsigned char {
#define ConditionTypeDefine(x, y, v) x = (v),
#include "Common/Common.def"
#undef ConditionTypeDefine
};

struct MBasicBlock;
struct MInstruction;
struct MFunction;
struct MModule;

struct Address {
  std::variant<int, Register> base; // global / stack object
  std::variant<int32_t, Register, Shift, RegisterList, MBasicBlock *, MFunction *, GlobalVariable *>
    offset_or_else;

  bool isPointerOrGlobal() const { return std::holds_alternative<Register>(base); }
  bool isStack() const { return std::holds_alternative<int>(base); }
  bool hasElseReg() const { return std::holds_alternative<Register>(offset_or_else); }
  bool hasShift() const { return std::holds_alternative<Shift>(offset_or_else); }
};

struct MInstruction : public ListNode<MInstruction> {
  // MBasicBlock *parent = nullptr;
  size_t id = 0; // used for possible register allocation.
  Opcode op = Opcode::NOP;
  Condition cond = Condition::CT_Any;
  Register ra = Register{-1};             // Rd, RdLo, Rn
  Register rb = Register{-1};             // Rm, Rn, RdHi
  Address rc = Address{Register{-1}, -1}; // Rs, Rm, Rn, imm, Operand2, label, function (esp. only declaration)

  static MInstruction *create();
};

struct MBasicBlock : public ListNode<MBasicBlock> {
  size_t id; // used for cfg linearization.
  List<MInstruction> insn;
  std::vector<MBasicBlock *> succ;
  std::vector<MBasicBlock *> pred;

  static MBasicBlock *create(MFunction *);
};

struct MFunction {
  // Linearly Numbered BB
  List<MBasicBlock> block;
  std::string name;
  bool refExternal;
  // stack frame sizes
  // 0 means temporary stack like push arguments.
  std::vector<FrameObject> objects;
  std::unordered_map<Value *, int> frame_info;
  uint32_t num_fix_object { 0 };
  size_t call_stack_args { 0 };
  std::vector<CalleeSaved> callee_saved; // r4, r5, r6, r7, r8, r9, r11, lr

  int vregs_id = Register::VREG_BEGIN;
  std::unordered_map<Value *, Register> value_map; // IR Value to register id.
  std::unordered_map<BasicBlock *, MBasicBlock *> bb_map;

  static MFunction *create(Function *, MModule *);
  inline MBasicBlock *GetBasicBlock(BasicBlock *B) { return bb_map.at(B);}
  // Return the id of frame object.
  int GetFrameObject(Value *V);
  // Return a given IR value is Alloca, Argument that have created a frame object.
  bool isFrameObject(Value *V);
  FrameObject *GetFrameByIndex(int index);
  // See LLVM MachineFrameInfo: FixedObject / StackObject.
  int CreateStackObject(Value *V, size_t size, bool isSpill = false);
  int CreateFixObject(Value *V, size_t size);
};

struct MModule {
  std::vector<MFunction *> function;
  std::vector<GlobalVariable *> global;
  std::unordered_map<Function *, MFunction *> func_map;
  MFunction *GetMFunction(Function *);
};

// \brief Hosting Lowered IR and MInsts after register allocation
struct MInstHost {
  MModule *root;
  MBasicBlock *machine_basic_block;
  MFunction *syoc_init_func;
  MInstHost() { root = new MModule; }
  [[nodiscard]] MModule *getModule() const { return root; }

  void setInsertPoint(MBasicBlock *mbb) { machine_basic_block = mbb; }
  void clearInsertPoint() { machine_basic_block = nullptr; }
  MInstruction *Other(Opcode op);
  MInstruction *RdRnRm(Opcode op, Register rd, Register rn, Register rm,
                       Condition cond = Condition::CT_Any);
  // pointer or global memory access with definite register
 MInstruction *RdRnImm(Opcode op, Register rd, Register rn, int imm,
                       Condition cond = Condition::CT_Any);
  // stack memory access with intermediate FrameObject representation,
  // will be lowered to [sp, #offset] afterward.
  MInstruction *RdRnImm(Opcode op, Register rd, int frame_idx, int32_t imm,
                        Condition cond = Condition::CT_Any);
  // load common imm.
  MInstruction *RdImm(Opcode op, Register rd, int32_t imm,
                      Condition cond = Condition::CT_Any);
  // load global variable 32bit address.
  MInstruction *RdImm(Opcode op, Register rd, GlobalVariable *globv,
                      Condition cond = Condition::CT_Any);
  MInstruction *RdRm(Opcode op, Register rd, Register rm,
                     Condition cond = Condition::CT_Any);
  MInstruction *RdRmRnRa(Opcode op, Register rd, Register rm,
                         Register rs, Register rn,
                         Condition cond = Condition::CT_Any);
  MInstruction *RdLoRdHiRnRm(Opcode op, Register lo, Register hi,
                             Register rm, Register rs,
                             Condition cond = Condition::CT_Any);
  MInstruction *RdRnOperand2(Opcode op, Register rd, Register rm,
                             Shift sf,
                             Condition cond = Condition::CT_Any);
  MInstruction *RdOperand2(Opcode op, Register rd, Shift sf,
                           Condition cond = Condition::CT_Any);
  MInstruction *RnOperand2(Opcode op, Register rn, Shift sf,
                           Condition cond = Condition::CT_Any);
  MInstruction *Label(Opcode op, MBasicBlock *label,
                      Condition cond = Condition::CT_Any);
  MInstruction *Label(Opcode op, MFunction *mfunc,
                      Condition cond = Condition::CT_Any);
  MInstruction *Rm(Opcode op, Register rm,
                   Condition cond = Condition::CT_Any);
  MInstruction *Reglist(Opcode op, RegisterList list,
                        Condition cond = Condition::CT_Any);
  MInstruction *FrameAddr(Opcode op, Register rd, int frame_fi,
                          Register offset_reg, int32_t offset_imm,
                          Condition cond = Condition::CT_Any);

};

// Operand2 Imm8
inline bool test_Imm8(int32_t Imm) {
  auto uImm = static_cast<uint32_t>(Imm);
  if (uImm < 0xffU) return true;
  for (int i = 1; i < 16; ++i) {
    uint32_t ror_uImm = (uImm << (i * 2)) | (uImm >> (32 - i * 2));
    if (ror_uImm < 0xffU) return true;
  }
  return false;
}

// mov/movw/movt Imm16
inline bool test_Imm16(int32_t Imm) {
  auto uImm = static_cast<uint32_t>(Imm);
  return uImm < 0xffffU;
}

inline bool test_LDR_STR_Imm_Offset(int32_t Imm) {
  return Imm >= -4095 && Imm <= 4095;
}

inline bool test_Imm_Pow2(int32_t Imm) {
  if (Imm == 0) return false;
  return (Imm & (Imm - 1)) == 0;
}

inline int32_t Imm_Lowbit(int32_t Imm) {
  return Imm & (-Imm);
}

// As compiled runtime library is compiled by thumb
// We should use blx to make appropriate function call.
inline bool isRuntimeFunction(std::string_view name) {
  return (name == "getint" || name == "getch" || name == "getfloat" ||
          name == "getarray" || name == "getfarray" ||
          name == "putint" || name == "putch" || name == "putfloat" ||
          name == "putarray" || name == "putfarray" ||
          name == "starttime" || name == "stoptime");
}

// Get Register Uses and Defs.
inline void
getUse(MInstruction *inst_iter, std::vector<Register *> &reg_use) {
  reg_use.clear();
  if (!inst_iter->ra.isInvalid() && inst_iter->op == Opcode::STR)
    reg_use.push_back(&inst_iter->ra);
  if (!inst_iter->rb.isInvalid()) reg_use.push_back(&inst_iter->rb);
  if (inst_iter->rc.isPointerOrGlobal() &&
      !std::get<Register>(inst_iter->rc.base).isInvalid())
    reg_use.push_back(&std::get<Register>(inst_iter->rc.base));
  if (inst_iter->rc.hasElseReg() &&
      !std::get<Register>(inst_iter->rc.offset_or_else).isInvalid())
    reg_use.push_back(&std::get<Register>(inst_iter->rc.offset_or_else));
  if (inst_iter->rc.hasShift()) {
    Register &shift_base = std::get<Shift>(inst_iter->rc.offset_or_else).reg;
    if (!shift_base.isInvalid()) reg_use.push_back(&shift_base);
  }
}

inline void
getDef(MInstruction *inst_iter, std::vector<Register *> &reg_def) {
  reg_def.clear();
  if (!inst_iter->ra.isInvalid() && inst_iter->op != Opcode::STR)
    reg_def.push_back(&inst_iter->ra);
}

} // namespace SyOC::ARMv7a