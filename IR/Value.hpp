#pragma once

#include <variant>

#include "Constant.hpp"
#include "Variable.hpp"

using namespace std;

struct Value {
  variant<Variable, IntegerConstant> v;
};
