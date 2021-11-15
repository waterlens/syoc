#pragma once
#include <string>
#include <string_view>

struct IntegerConstant {
  int literal;

  std::string to_string() const { return std::to_string(literal); }
};