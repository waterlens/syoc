#pragma once

#include "Value.hpp"
#include <fmt/format.h>
#include <string_view>
#include <variant>

enum OpType {
  NONE,
  PHI,
  MUL,
  DIV,
  MOD,
  ADD,
  SUB,
  LT,
  GT,
  LE,
  GE,
  EQ,
  NE,
  LAND,
  LOR,
  BRANCH,
  END,
};

inline constexpr std::string_view op_name[]{
  "(none)", "phi", "mul", "div", "mod",  "add", "sub", "lt",   "gt",
  "le",     "ge",  "eq",  "neq", "land", "lor", "br",  "(end)"};

struct BasicBlock;

struct Instruction {
  OpType type;
  std::variant<std::pair<Value *, Value *>,
               std::pair<BasicBlock *, BasicBlock *>>
    operands;
  Instruction *next, *prev;
  std::string to_string() const;
};