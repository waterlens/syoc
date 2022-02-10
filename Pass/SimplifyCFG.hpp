#pragma once

#include "PassBase.hpp"
#include "PassCollection.hpp"

#include <unordered_map>

class SimplifyCFG : public SSATransformation<SimplifyCFG> {
  static void clearExtraJump(IRHost &host, BasicBlock *bb);
  static void removeDanglingBB(IRHost &host);

public:
  SimplifyCFG() = default;
  [[nodiscard]] static std::string_view getName() { return "Simplify CFG"; }
  void operator()(IRHost &host);
};