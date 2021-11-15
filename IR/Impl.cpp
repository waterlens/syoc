#include <fmt/format.h>

#include "Value.hpp"
#include "Variable.hpp"

std::string Variable::to_string() const {
  string s = fmt::format("{} {} ", ty.to_string(), name);
  s += "{";
  s += dimension->to_string();
  s += "}";
  return s;
}