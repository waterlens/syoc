#pragma once

#include <memory>
#include <stdexcept>
#include <string_view>
#include <unordered_map>
#include <utility>
#include <vector>

#include "Value.hpp"

struct Scope {
  Scope *top;
  vector<Scope *> children;
  unordered_map<string_view, Value> table;

  Scope &entry();

  Scope &leave();
};