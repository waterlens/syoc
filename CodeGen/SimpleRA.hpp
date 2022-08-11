#pragma once

/// \brief Trivial Register Allocator

#include "IR/ASM.hpp"

#include <set>
#include <queue>

namespace SyOC {
namespace ARMv7a {

struct SpillInfo {
  int FrameIndex;
};

struct LiveInterval {
  size_t In;
  size_t Out;
  // strategy: spill further live-out
  bool operator <(const LiveInterval &other) const {
    return Out > other.Out;
  }
};

class SimpleRA {
private:
  // alloc
  std::unordered_map<int, int> vreg2preg;
  // virtual register rough live interval statistics.
  std::set<int> free_int_reg;
  std::set<int> free_float_reg;
  std::vector<std::pair<LiveInterval, int> > free_intervals;
  // the live intervals occupying a physical register.
  std::set<std::pair<LiveInterval, int> > occupied_intervals;
  // spills.
  std::unordered_map<int, SpillInfo> spiller;
  static constexpr int default_aux_reg = Register::r0;

  int getFreeReg(Register::Type);
  void collectFreeReg(int RegId, Register::Type);
  void getRawLiveness(MFunction *);
  void CFGLinearization(MInstHost &host);
  void trySpill(int RegId, LiveInterval, MFunction *);
  void rewrite(MFunction *, MInstHost *);
public:
  SimpleRA() = default;
  static std::string_view getName() { return "Simple Register Allocation"; }
  void operator()(MInstHost &);
};

} // end namespace ARMv7a
} // end namespace SyOC
