#include <fmt/format.h>

#include "BasicBlock.hpp"
#include "Instruction.hpp"
#include "Scope.hpp"
#include "Type.hpp"
#include "Value.hpp"
#include "Variable.hpp"

Scope &Scope::entry() {
  auto newScope = new Scope();
  newScope->top = this;
  children.push_back(newScope);
  return *newScope;
}

Scope &Scope::leave() {
  if (!top)
    throw runtime_error("can't leave top scope");
  return *top;
}

std::string Variable::to_string() const {
  string s = fmt::format("{} {} ", ty.to_string(), name);
  s += "{";
  s += dimension->to_string();
  s += "}";
  return s;
}

std::string Value::to_string() const {
  if (v.index() == 0) {
    return get<0>(v).to_string();
  } else if (v.index() == 1) {
    return get<1>(v).to_string();
  } else {
    string ret = "{";
    for (auto &i : get<2>(v)) ret += i.to_string() + " ";
    ret += "}";
    return ret;
  }
}

bool Type::isDeclarationSpecifiersValid() const {
  switch (ty_spec) {
  case INT:
  case VOID:
    return true;
  default:
    return false;
  }
}

bool Type::isVoid() const { return ty_spec & VOID; }

std::string_view Type::to_string() const {
  if (ty_qual == CONST) {
    return std::string_view(ty_spec & INT ? "const int" : "const void");
  } else {
    return std::string_view(ty_spec & INT ? "int" : "void");
  }
}

std::string Instruction::to_string() const {
  if (type >= NONE && type < END) {
    if (operands.index() == 0)
      return fmt::format("{} {} {}", op_name[type],
                         get<0>(operands).first->to_string(),
                         get<0>(operands).second->to_string());
    if (operands.index() == 1)
      return fmt::format("{} {} {}", op_name[type],
                         get<1>(operands).first->get_id_string(),
                         get<1>(operands).second->get_id_string());
  }
  return "";
}

std::string BasicBlock::to_string() const {
  std::string s;
  s += get_id_string();
  s.push_back('\n');
  auto p = begin;
  while (p != end) {
    s += p->to_string();
    s.push_back('\n');
    p = p->next;
  }
  return s;
}