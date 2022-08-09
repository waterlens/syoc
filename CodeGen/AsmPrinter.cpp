#include "CodeGen/AsmPrinter.hpp"
#include "Pass/AssignIdentityHelper.hpp"
#include <fmt/core.h>
#include <fmt/format.h>
#include <fmt/os.h>
using namespace SyOC::ARMv7a;

// register names
static const char *RegNames[] = {
#define TargetIntegralRegister(x, y, z) #x,
#define TargetFloatRegister(x, y, z) #x,
#include "Common/TargetInfo.def"
};

static const char *getMInstName(Opcode Op) {
  switch (Op) {
#define ARMv7aOpcodeDefine(type, name, format) \
  case Opcode::type: return name;
#include "Common/Common.def"
  }
}

static const char *getCondName(Condition cond) {
  switch (cond) {
#define ConditionTypeDefine(type, name, num) \
  case Condition::type: return name;
#include "Common/Common.def"
  }
}

static const char *getShiftName(Shift::Type stype) {
  switch (stype) {
#define  ShiftTypeDefine(type, name) \
  case Shift::Type::type: return name;
#include "Common/Common.def"
  }
}

static std::string getRegName(Register &reg) {
  if (reg.isInvalid()) return "%invalid:gpr";
  if (reg.isVirtual())
    return fmt::format("%{:d}:gpr", reg.id - Register::VREG_BEGIN);
  return RegNames[reg.id];
}

static std::string dumpIF_Other(MInstruction *minst) {
  assert(minst->op == Opcode::NOP || minst->op == Opcode::CLEARUSE);
  return "\tnop\n";
}

static std::string dumpIF_RdRnOperand2(MInstruction *minst) {
  Shift shift = std::get<Shift>(minst->rc.offset_or_else);
  // pure immediate
  if (shift.reg.isInvalid()) {
    return fmt::format("\t{}{}\t{}, {}, #{:d}\n",
                       getMInstName(minst->op), getCondName(minst->cond),
                       getRegName(minst->ra), getRegName(minst->rb),
                       shift.imm);
  }
  // shift
  if (shift.imm == 0) {
    return fmt::format("\t{}{}\t{}, {}, {}\n",
                       getMInstName(minst->op), getCondName(minst->cond),
                       getRegName(minst->ra), getRegName(minst->rb),
                       getRegName(shift.reg));
  }
  return fmt::format("\t{}{}\t{}, {}, {}, {}, #{:d}\n",
                     getMInstName(minst->op), getCondName(minst->cond),
                     getRegName(minst->ra), getRegName(minst->rb),
                     getRegName(shift.reg), getShiftName(shift.type), shift.imm);
}

static std::string dumpIF_RdRnImm(MInstruction *minst) {
  // unlowered fram addressing.
  if (minst->rc.isStack()) {
    return fmt::format("\t{}{}\t{}, #fi:{:d}, {:d}\n",
                       getMInstName(minst->op), getCondName(minst->cond),
                       getRegName(minst->ra),
                       std::get<int>(minst->rc.base), std::get<int32_t>(minst->rc.offset_or_else));
  }
  Register base = std::get<Register>(minst->rc.base);
  int32_t imm = std::get<int32_t>(minst->rc.offset_or_else);
  if (minst->op == Opcode::LDR || minst->op == Opcode::STR)
    return fmt::format("\t{}{}\t{}, [{}, #{:d}]\n",
                       getMInstName(minst->op), getCondName(minst->cond),
                       getRegName(minst->ra),
                       getRegName(base), imm);
  return fmt::format("\t{}{}\t{}, {} #{:d}\n",
                     getMInstName(minst->op), getCondName(minst->cond),
                     getRegName(minst->ra),
                     getRegName(base), imm);
}

static std::string dumpIF_RdOperand2(MInstruction *minst) {
  Shift shift = std::get<Shift>(minst->rc.offset_or_else);
  // pure immediate
  if (shift.reg.isInvalid()) {
    return fmt::format("\t{}{}\t{}, #{:d}\n",
                       getMInstName(minst->op), getCondName(minst->cond),
                       getRegName(minst->ra), shift.imm);
  }
  // shift
  if (shift.imm == 0) {
    return fmt::format("\t{}{}\t{}, {}\n",
                       getMInstName(minst->op), getCondName(minst->cond),
                       getRegName(minst->ra), getRegName(shift.reg));
  }
  return fmt::format("\t{}{}\t{}, {}, {}, #{:d}\n",
                     getMInstName(minst->op), getCondName(minst->cond),
                     getRegName(minst->ra),
                     getRegName(shift.reg), getShiftName(shift.type), shift.imm);
}

static std::string dumpIF_RnOperand2(MInstruction *minst) {
  Shift shift = std::get<Shift>(minst->rc.offset_or_else);
  // pure immediate
  if (shift.reg.isInvalid()) {
    return fmt::format("\t{}{}\t{}, #{:d}\n",
                       getMInstName(minst->op), getCondName(minst->cond),
                       getRegName(minst->rb), shift.imm);
  }
  // shift
  if (shift.imm == 0) {
    return fmt::format("\t{}{}\t{}, {}\n",
                       getMInstName(minst->op), getCondName(minst->cond),
                       getRegName(minst->rb), getRegName(shift.reg));
  }
  return fmt::format("\t{}{}\t{}, {}, {}, #{:d}\n",
                     getMInstName(minst->op), getCondName(minst->cond),
                     getRegName(minst->rb),
                     getRegName(shift.reg), getShiftName(shift.type), shift.imm);
}

static std::string dumpIF_RdRnRm(MInstruction *minst) {
  Register rm = std::get<Register>(minst->rc.base);
  return fmt::format("\t{}{}\t{}, {}, {}\n",
                     getMInstName(minst->op), getCondName(minst->cond),
                     getRegName(minst->ra), getRegName(minst->rb), getRegName(rm));
}

static std::string dumpIF_RdRmRnRa(MInstruction *minst) {
  Register rn = std::get<Register>(minst->rc.base);
  Register ra = std::get<Register>(minst->rc.offset_or_else);
  return fmt::format("\t{}{}\t{}, {}, {}, {}\n",
                     getMInstName(minst->op), getCondName(minst->cond),
                     getRegName(minst->ra), getRegName(minst->rb), rn.id, ra.id);
}

static std::string dumpIF_RdLoRdHiRnRm(MInstruction *minst) {
  Register rn = std::get<Register>(minst->rc.base);
  Register rm = std::get<Register>(minst->rc.offset_or_else);
  return fmt::format("\t{}{}\t{}, {}, {}, {}\n",
                     getMInstName(minst->op), getCondName(minst->cond),
                     getRegName(minst->ra), getRegName(minst->rb),
                     getRegName(rn), getRegName(rm));
}

static std::string dumpIF_Label(MInstruction *minst) {
  if (minst->op == Opcode::B) {
    MBasicBlock *mbb = std::get<MBasicBlock *>(minst->rc.offset_or_else);
    return fmt::format("\t{}{}\t.L{:d}\n", getMInstName(minst->op),
                       getCondName(minst->cond), mbb->id);
  }
  if (minst->op == Opcode::BL || minst->op == Opcode::BLX) {
    MFunction *mfunc = std::get<MFunction *>(minst->rc.offset_or_else);
    return fmt::format("\t{}{}\t{}\n", getMInstName(minst->op),
                       getCondName(minst->cond), mfunc->name);
  }
  return "\tnop @unrecognized branch\n";
}

static std::string dumpIF_RdImm(MInstruction *minst) {
  // we either use movw/movt (not generate LDR, =expr in isel),
  // or generate suitable literal pool basic block in a later pass.
  if (minst->op == Opcode::LDR_PC) {
    MBasicBlock *literal_pool = std::get<MBasicBlock *>(minst->rc.offset_or_else);
    return fmt::format("\tldr{}\t{}, .L{:d}",
                       getCondName(minst->cond), minst->ra.id, literal_pool->id);
  }
  // Load GlobalVariable.
  using SyOC::GlobalVariable;
  if (std::holds_alternative<int32_t>(minst->rc.offset_or_else)) {
    int32_t imm = std::get<int32_t>(minst->rc.offset_or_else);
    return fmt::format("\t{}{}\t{}, #{:d}\n", getMInstName(minst->op),
                       getCondName(minst->cond), getRegName(minst->ra), imm);
  }
  auto *globv = std::get<GlobalVariable *>(minst->rc.offset_or_else);
  if (minst->op == Opcode::MOVW)
    return fmt::format("\t{}{}\t{}, #:lower16:{}\n", getMInstName(minst->op),
                      getCondName(minst->cond), getRegName(minst->ra), globv->name);
  return fmt::format("\t{}{}\t{}, #:upper16:{}\n", getMInstName(minst->op),
                     getCondName(minst->cond), getRegName(minst->ra), globv->name);
}

static std::string dumpIF_RdRm(MInstruction *minst) {
  assert(minst->op == Opcode::CLZ || minst->op == Opcode::CPY);
  return fmt::format("\t{}{}\t{}, {}\n",
                     getMInstName(minst->op), getCondName(minst->cond),
                     getRegName(minst->ra), getRegName(minst->rb));
}

static std::string dumpIF_Rm(MInstruction *minst) {
  assert(minst->op == Opcode::BX);
  return fmt::format("\tbx\tlr\n");
}

static std::string dumpIF_Reglist(MInstruction *minst) {
  RegisterList list = std::get<RegisterList>(minst->rc.offset_or_else);
  bool first_reg = true;
  std::string asm_format = fmt::format("\t{}\t{", getMInstName(minst->op));
  for (int i = 0; i < RegisterList::RegCount; ++i) {
    if (list.ls & (1 << i)) {
      if (first_reg) { asm_format += RegNames[i]; first_reg = false; }
      asm_format.append(" ,");
      asm_format.append(RegNames[i]);
    }
  }
  asm_format += "}\n";
  assert(!first_reg);
  return asm_format;
}

static std::string dumpIF_FrameAddr(MInstruction *minst) {
  return fmt::format("\tframe\t{}, #fi:{:d} offset {}, {:d}\n",
                     getRegName(minst->ra),
                     std::get<int>(minst->rc.base),
                     getRegName(minst->rb),
                     std::get<int32_t>(minst->rc.offset_or_else));
}

void AsmPrinter::dumpMInst(MInstruction *minst) {
  Format format = get_op_format(minst->op);
  switch (format) {
#define ARMv7aInsnFormatDefine(type) \
  case Format::type: buffer += dump##type(minst); break;
#include "Common/Common.def"
  }
}

void AsmPrinter::dumpMBasicBlock(MBasicBlock *mbb) {
  buffer += fmt::format(".L{:d}:\n", mbb->id);
  for (auto &minst : mbb->insn) {
    dumpMInst(&minst);
  }
}

void AsmPrinter::dumpMFunction(MFunction *mfunc) {
  static size_t func_count = 0;
  // align 2 targets arm 32bit
  // align 1 targets thumb 16bit
  buffer += "\t.align  2\n";
  buffer += fmt::format("\t.global {}\n", mfunc->name);
  // use unified GNU ARM asm syntax and arm code.
  // GCC generate thumb code by default.
  buffer += "\t.syntax unified\n"
            "\t.arm\n";
  buffer += fmt::format("\t.type\t{}, %function\n", mfunc->name);
  buffer += fmt::format("{}:\n", mfunc->name);
  // print frame info.
  for (int i = 0; i < mfunc->objects.size(); ++i)
    buffer += fmt::format("// #fi:{:d} stack:%{:d}, size = {:d}\n",
                          i - mfunc->num_fix_object, i, mfunc->objects[i].Size);
  // dump Machine BB.
  for (auto &mbb : mfunc->block) {
    dumpMBasicBlock(&mbb);
  }
  buffer += fmt::format("\t.size\t{}, .-{}\n", mfunc->name, mfunc->name);
}

// dump all global variable to .bss segment
void AsmPrinter::dumpGlobalVariable(MInstHost &mhost) {
  for (auto *globv : mhost.root->global) {
    buffer += fmt::format("\t.global\t{}\n", globv->name);
    buffer += "\t.bss\n" // dump to .bss
              "\t.align\t2\n";
    buffer += fmt::format("\t.type\t{}, %object\n", globv->name);
    buffer += fmt::format("\t.size\t{}, {:d}\n", globv->name, globv->capacity);
    buffer += fmt::format("{}:\n", globv->name);
    buffer += fmt::format("\t.space\t{:d}\n", globv->capacity);
  }
}

void AsmPrinter::dumpAsm(MInstHost &host) {
  buffer += "\t.arch armv7-a\n"
            "\t.fpu neon\n";
  // dump instructions.
  buffer += "\t.text\n";
  for (auto *mfunc : host.root->function) {
    dumpMFunction(mfunc);
  }
  // dump global variable.
  dumpGlobalVariable(host);
  // declare stack
  buffer += "\t.section        .note.GNU-stack,\"\",%progbits\n"
            "\t.ident\t\"SyOC: 1.0.0\"\n";
  auto out =
    fmt::buffered_file(filename, "wb");
  out.print("{}", buffer);
  fmt::print("Dump Asm '{}'\n", filename);
}

void AsmPrinter::print(std::string_view outfile, MInstHost *host) {
  buffer.clear();
  filename = outfile;
  assignMIdentity(*host);
  dumpAsm(*host);
}
