#pragma once

#include "IR/ASM.hpp"

namespace SyOC {
namespace ARMv7a {

class MachineDCE {
  std::vector<MInstruction *> work_list;
  void removeDeadMov(MInstHost &host);
public:
  static std::string_view getName() { return "Machine DCE"; }
  void operator()(MInstHost &host);
};

}
} // end namespace SyOC