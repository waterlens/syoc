#pragma once

#include "IR/IR.hpp"
#include "PassBase.hpp"
#include "Tree/Tree.hpp"
#include "Util/Filter.hpp"
#include "Util/TrivialValueVector.hpp"

#include <unordered_map>
#include <vector>

class LocalCopyPropagation : public SSATransformation<LocalCopyPropagation> {
private:
  static void eliminateLocalLoad(IRHost &host);
public:
  LocalCopyPropagation() = default;
  [[nodiscard]] static std::string_view getName() {
    return "Local Copy Propagation";
  }
  void operator()(IRHost &host) { eliminateLocalLoad(host); }
};