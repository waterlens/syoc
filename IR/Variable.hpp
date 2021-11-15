#pragma once
#include <memory>
#include <string_view>
#include <vector>

#include "Constant.hpp"
#include "Type.hpp"

using namespace std;

struct Value;
struct Variable {
  Type ty;
  string_view name;
  std::shared_ptr<Value> dimension;
  std::string to_string() const;
};
