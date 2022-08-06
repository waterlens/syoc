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
  std::set<int> spilled;
  std::vector<SpillInfo> spiller;
  std::unordered_map<int, bool> loaded;

  void insertSpill();
public:
  SimpleRA() = default;
  static std::string_view getName() { return "Simple Register Allocation"; }
  void operator()(MInstHost &);
};

} // end namespace ARMv7a
} // end namespace SyOC
