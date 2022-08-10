#pragma once

#include "IR/IR.hpp"

namespace SyOC {

class IRLegalize {
private:
  std::vector<Instruction *> work_list;
  Function *aeabi_idiv;
  Function *aeabi_idivmod;
  void virtualExtension(Function *, IRHost &host);
public:
  static std::string_view getName() { return "IR Legalize"; }
  void operator()(IRHost &host);
};

} // end namespace SyOC
