#pragma once

#include <fmt/format.h>
#include <memory>
#include <stdexcept>
#include <string_view>
#include <unordered_map>
#include <utility>
#include <variant>
#include <vector>

#define OpcodeDefine(x) x,
enum OpType {
#include "IR.def"
};

#define OpcodeDefine(x) #x,
inline constexpr std::string_view op_name[]{
#include "IR.def"
};

#define ValueTypeDefine(x) x,
enum ValueType {
#include "IR.def"
};

#define ValueTypeDefine(x) #x,
inline constexpr std::string_view value_type_name[]{
#include "IR.def"
};

#define TypeSpecifierDefine(x, y) x = (y),
enum TypeSpecifier {
#include "IR.def"
};

#define TypeSpecifierDefine(x, y) #x,
inline constexpr std::string_view type_spec_name[]{
#include "IR.def"
};

#define TypeQualifierDefine(x, y) x = (y),
enum TypeQualifier {
#include "IR.def"
};

#define TypeQualifierDefine(x, y) #x
inline constexpr std::string_view type_qual_name[]{
#include "IR.def"
};

struct Type;
struct Value;

struct User;
struct BasicBlock;

struct Instruction;
struct Variable;
struct Constant;
struct Function;

struct Type {
  std::string to_string() { return ""; }
};

struct Value {
  std::string to_string() { return ""; }
};

struct BasicBlock : public Value {};

struct User : public Value {};

struct Variable {};

struct Instruction : public User {};

struct Constant : public User {};