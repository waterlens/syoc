
#include "SimpleRA.hpp"
#include <vector>

using namespace SyOC::ARMv7a;


static bool used[RegisterList::RegCount] = { false };

static void spill();

static void setAllAvailable() {
  memset(used, 0, sizeof(used));
}

static int getAvailableIntReg() {
  for (int i = Register::r4; i <= Register::r10; ++i)
    if (!used[i]) {
      used[i] = true;
      return i;
    }
  return -1;
}

static int getAvailableFloatReg() {
  for (int i = Register::s4; i <= Register::s31; ++i)
    if (!used[i]) {
      used[i] = true;
      return i;
    }
  return -1;
}

static int getAvailableReg(Register::Type type) {
  return type == Register::Int ? getAvailableIntReg() : getAvailableFloatReg();
}

static std::vector<Register *>
getUse(MInstruction *minst) {
  std::vector<Register *> Result;
  if (!minst->ra.isInvalid()) Result.push_back(&minst->ra);
  if (!minst->rb.isInvalid()) Result.push_back(&minst->rb);
  if (minst->rc.isPointerOrGlobal())
    Result.push_back(&std::get<Register>(minst->rc.base));
  if (minst->rc.hasElseReg())
    Result.push_back(&std::get<Register>(minst->rc.offset_or_else));
  if (minst->rc.hasShift()) {
    Register &shift_base = std::get<Shift>(minst->rc.offset_or_else).reg;
    if (!shift_base.isInvalid()) Result.push_back(&shift_base);
  }
  return Result;
}

void SimpleRA::operator()(MInstHost &mhost) {
  for (MFunction *mf : mhost.root->function) {
    for (auto mbb = mf->block.begin(); mbb != mf->block.end(); ++mbb) {
      for (auto minst = mbb->insn.begin(); minst != mbb->insn.end(); ++minst) {
        auto Uses = getUse(&*minst);
        for (Register *reg : Uses) {
          if (!reg->isVirtual())
            continue ;
          auto map_iter = reg_map.find(reg->id);
          if (map_iter != reg_map.end()) {
            reg->id = map_iter->second;
          } else {
            int alloc_id = getAvailableReg(reg->type);
            reg_map.insert(std::make_pair(reg->id, alloc_id));
            fmt::print("allocate vreg:{:d} to id:{:d}\n", reg->id, alloc_id);
            assert(alloc_id != -1);
            reg->id = alloc_id;
          }
        }
        // We assume after select mem SSA, store means assign a value,
        // all intermediate result will not be used again.
        if (minst->op == Opcode::STR || minst->op == Opcode::CMP) {
          setAllAvailable();
          reg_map.clear();
        }
      }
    }
  }
}