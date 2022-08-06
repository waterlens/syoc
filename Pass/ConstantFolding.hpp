#pragma once

#include "IR/IR.hpp"

namespace SyOC {

class ConstantFolding {
private:
  std::vector<Instruction *> work_list;
  void foldConstant(IRHost &host);
  void foldIdentity(IRHost &host);
  void deadCodeElimination();
public:
  static std::string_view getName() { return "Constant Folding"; }
  void operator()(IRHost &host);
};

} // end namespace SyOC