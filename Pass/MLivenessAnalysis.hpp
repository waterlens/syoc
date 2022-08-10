#pragma once
#include "IR/ASM.hpp"
#include <unordered_map>
#include <unordered_set>

namespace SyOC {
namespace ARMv7a {

class MLivenessAnalysis {
private:
  std::unordered_map<int, std::unordered_set<int> > LiveIn;
  std::unordered_map<int, std::unordered_set<int> > LiveOut;
public:
  static std::string_view getName() { return "Virtual Register Liveness Analysis"; }
  inline std::unordered_set<int>
  &getLiveIns(int MInstID) {
    return LiveIn.at(MInstID);
  }
  inline std::unordered_set<int>
  &getLiveOuts(int MInstID) {
    return LiveOut.at(MInstID);
  }
  void operator()(MFunction *);
};

} // end namespace ARMv7a
} // end namespace SyOC
