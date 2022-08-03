#pragma once

/// \brief Trivial Register Allocator

#include "IR/ASM.hpp"

#include <set>

namespace SyOC {
namespace ARMv7a {

struct SpillInfo {
};

class SimpleRA {
private:
  // alloc
  std::unordered_map<int, int> reg_map;
  // spills.
  std::set<Register> spilled;
  std::vector<SpillInfo> spiller;

  void insertSpill();
public:
  SimpleRA() = default;
  void operator()(MInstHost &);
};

} // end namespace ARMv7a
} // end namespace SyOC
