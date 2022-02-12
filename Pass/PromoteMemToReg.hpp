#pragma once

#include "PassBase.hpp"

class PromoteMemToReg : public SSATransformation<PromoteMemToReg> {
public:
  PromoteMemToReg() = default;
  [[nodiscard]] static std::string_view getName() {
    return "Promote Memory to Register";
  }
  void operator()(IRHost &host);
};