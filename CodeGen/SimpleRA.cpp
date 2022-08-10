
#include "SimpleRA.hpp"
#include "Pass/AssignIdentityHelper.hpp"
#include "Pass/TraversalHelper.hpp"
#include <vector>
#include <limits>


using namespace SyOC::ARMv7a;

// Used to
static int preg2vreg[RegisterList::RegCount];
static bool needCalleeSave[RegisterList::RegCount] = { false };
static std::vector<int> default_free_int_reg = {
  Register::r4, Register::r5, Register::r6, Register::r7, Register::r8,
  Register::r9, Register::r10
};

static bool checkReDef(const std::vector<Register *> &reg_def,
                const std::vector<Register *> &reg_use)
{
  for (auto *used : reg_use)
    for (auto *defed : reg_def)
      if (used->id == defed->id) return true;
  return false;
}


void SimpleRA::rewrite(MFunction *MF, MInstHost *MHost) {
  std::vector<Register *> reg_use;
  std::vector<Register *> reg_def;
  MHost->clearInsertPoint();
  for (auto &MBB : MF->block) {
    for (auto &MI : MBB.insn) {
      Register AuxReg {Register::r0, Register::Type::Int};
      getUse(&MI, reg_use);
      for (auto *reg : reg_use) {
        if (!reg->isVirtual())
          continue;
        // spilled, reload
        auto spill_iter = spiller.find(reg->id);
        if (spill_iter != spiller.end()) {
          auto *Reload = MHost->RdRnImm(Opcode::LDR, AuxReg,
                                        spill_iter->second.FrameIndex, 0);
          MI.insert_before(Reload);
          reg->id = AuxReg.id;
          AuxReg.id++;
        } else {
          int preg_id = vreg2preg.at(reg->id);
          if (!needCalleeSave[preg_id])
            needCalleeSave[preg_id] = true;
          reg->id = preg_id;
        }
      }

      getDef(&MI, reg_def);
      for (auto *reg : reg_def) {
        if (!reg->isVirtual())
          continue;
        // spilled, spill to memory
        auto spill_iter = spiller.find(reg->id);
        if (spill_iter != spiller.end()) {
          auto *Reload = MHost->RdRnImm(Opcode::STR, AuxReg,
                                        spill_iter->second.FrameIndex, 0);
          MI.insert_after(Reload);
          reg->id = AuxReg.id;
          AuxReg.id++;
        } else {
          int preg_id = vreg2preg.at(reg->id);
          if (!needCalleeSave[preg_id])
            needCalleeSave[preg_id] = true;
          reg->id = preg_id;
        }
      }
    }
  }
}

void SimpleRA::trySpill(int RegId, LiveInterval Interval, MFunction *MF) {
  auto FurtherLive = occupied_intervals.begin();
  // Spill a reg in use.
  if (Interval.Out < FurtherLive->first.Out) {
    vreg2preg[RegId] = vreg2preg.at(FurtherLive->second);
    int FrameIndex = MF->CreateStackObject(nullptr, 4, true);
    spiller.insert(std::make_pair(FurtherLive->second,
                                  SpillInfo{FrameIndex}));
    occupied_intervals.erase(FurtherLive);
    occupied_intervals.insert(std::make_pair(Interval, RegId));
  } else {
    int FrameIndex = MF->CreateStackObject(nullptr, 4, true);
    spiller.insert(std::make_pair(RegId, SpillInfo{FrameIndex}));
  }
}

int SimpleRA::getFreeReg(Register::Type type) {
  if (type == Register::Type::Int && !free_int_reg.empty()) {
    auto int_reg_iter = free_int_reg.begin();
    int int_reg_id = *int_reg_iter;
    free_int_reg.erase(int_reg_iter);
    return int_reg_id;
  }
  if (type == Register::Type::Float && !free_float_reg.empty()){
    auto float_reg_iter = free_float_reg.begin();
    int float_reg_id = *float_reg_iter;
    free_float_reg.erase(float_reg_iter);
    return float_reg_id;
  }
  return -1;
}

void SimpleRA::collectFreeReg(int RegId, Register::Type type) {
  if (RegId == -1) return;
  if (type == Register::Type::Int) free_int_reg.insert(RegId);
  else free_float_reg.insert(RegId);
}

void SimpleRA::operator()(MInstHost &mhost) {
  CFGLinearization(mhost);
  assignMIdentity(mhost);
  std::vector<Register *> reg_uses;
  std::vector<Register *> reg_defs;
  /// @attention: We assume after select mem SSA, store means assign a value,
  /// all intermediate result will not be preg2vreg again.
  /// But it's wrong.
  /// @code
  ///     void %7 <- store i32* %4, i32 #p //
  ///     i32 %8 <- sub i32 #p, 1 // %10 %9
  ///     void %9 <- store i32* %4, i32 %8 //
  ///     void %10 <- ret i32 %8 //
  for (MFunction *mf : mhost.root->function) {
    if (mf->refExternal)
      continue;
    memset(needCalleeSave, 0, sizeof(needCalleeSave));
    occupied_intervals.clear();
    free_intervals.clear();
    free_int_reg.clear();
    free_int_reg.insert(default_free_int_reg.begin(),
                        default_free_int_reg.end());
    spiller.clear();

    getRawLiveness(mf);
    for (const auto &live : free_intervals) {
      int reg_in = live.first.In;
      std::vector<decltype(occupied_intervals.begin())> work_list;
      for (auto occupy_iter = occupied_intervals.begin();
           occupy_iter != occupied_intervals.end(); ++occupy_iter) {
        // a dead vreg.
        if (occupy_iter->first.Out <= reg_in) {
          int reg_id = vreg2preg.at(occupy_iter->second);
          collectFreeReg(reg_id, Register::Type::Int);
          // occupied_intervals.erase(occupy_iter);
          work_list.push_back(occupy_iter);
        }
      }
      for (auto &dead_interval : work_list)
        occupied_intervals.erase(dead_interval);

      int free_reg_id = getFreeReg(Register::Type::Int);
      if (free_reg_id != -1) {
#ifndef NDEBUG
        fmt::print("allocate vreg:{} to preg:{}\n", live.second, free_reg_id);
#endif
        vreg2preg[live.second] = free_reg_id;
        occupied_intervals.insert(live);
      } else
        trySpill(live.second, live.first, mf);
    }
    rewrite(mf, &mhost);
    // A no-use CalleeSaved Info.
    for (int i = Register::r4; i < RegisterList::RegCount; ++i)
      if (needCalleeSave[i] && i > Register::r3) {
        CalleeSaved Info;
        Info.Reg = {i};
        Info.Locate.FrameIdx = 0;
        mf->callee_saved.push_back(Info);
      }
  }
}

void SimpleRA::getRawLiveness(MFunction *mfunc) {
  std::unordered_map<int, LiveInterval> temp_liveness;
  std::vector<Register *> reg_uses;
  std::vector<Register *> reg_defs;
  for (auto &mbb : mfunc->block) {
    for (auto inst_iter = mbb.insn.begin(), end_iter = mbb.insn.end();
         inst_iter != end_iter; ++ inst_iter)
    {
      getUse(inst_iter.base(), reg_uses);
      for (auto *reg : reg_uses) {
        if (!reg->isVirtual())
          continue;
        auto map_iter = temp_liveness.find(reg->id);
        assert(map_iter != temp_liveness.end());
        int cur_out = map_iter->second.Out;
        if (cur_out < inst_iter->id)
          map_iter->second.Out = inst_iter->id;
      }
      getDef(inst_iter.base(), reg_defs);
      for (auto *reg : reg_defs) {
        if (!reg->isVirtual())
          continue;
        auto map_iter = temp_liveness.find(reg->id);
        if (map_iter != temp_liveness.end()) {
          int cur_in = map_iter->second.In;
          if (cur_in > inst_iter->id)
            map_iter->second.In = inst_iter->id;
        } else
          temp_liveness.insert(std::make_pair(reg->id,
                        LiveInterval{inst_iter->id,0}));
      }
    }
  }
  for (auto &iter : temp_liveness) {
    free_intervals.push_back(std::make_pair(iter.second, iter.first));
  }
  std::sort(free_intervals.begin(), free_intervals.end(),
            [](const auto &lhs, const auto &rhs) { return lhs.first.In < rhs.first.In;});
}

void SimpleRA::CFGLinearization(MInstHost &host) {
    /// @TODO
}