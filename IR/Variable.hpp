#include "IR/Constant.hpp"
#include "Type.hpp"
#pragma once
#include <string_view>
#include <vector>

#include "Constant.hpp"

using namespace std;

struct Variable {
  Type ty;
  string_view name;
  vector<IntegerConstant> dimension;
};
