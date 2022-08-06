#pragma once

#include "IR/ASM.hpp"

namespace SyOC {
namespace ARMv7a {

class PeepHole {
private:
  std::vector<MInstruction *> work_list;
  void mergeCopy(MBasicBlock *, MInstHost *);
  void removeMemoryAccess(MBasicBlock *, MInstHost *);
public:
  static std::string_view getName() { return "PeepHole Optimization";}
  void operator()(MInstHost &host);
};

}
}

