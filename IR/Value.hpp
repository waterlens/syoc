#pragma once

#include <variant>
#include <string>

#include "Constant.hpp"
#include "Variable.hpp"

using namespace std;

struct Value {
  variant<Variable, IntegerConstant, vector<Value>> v;
  std::string to_string() const;
};
