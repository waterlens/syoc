#pragma once

#include <variant>
#include <string>

#include "Constant.hpp"
#include "Variable.hpp"

using namespace std;

struct Value {
  variant<Variable, IntegerConstant, vector<Value>> v;
  std::string to_string() const {
    if (v.index() == 0) {
      return get<0>(v).to_string();
    } else if (v.index() == 1) {
      return get<1>(v).to_string();
    } else {
      string ret = "{";
      for (auto &i : get<2>(v))
        ret += i.to_string() + " ";
      ret += "}";
      return ret;
    }
  }
};
