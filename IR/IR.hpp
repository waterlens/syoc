#pragma once

#include <array>
#include <cstddef>
#include <cstdint>
#include <fmt/format.h>
#include <limits>
#include <memory>
#include <stdexcept>
#include <string>
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

#define ConstantTypeDefine(x) x,
enum ConstantType {
#include "IR.def"
};

#define COnstantTypeDefine(x) #x,
inline constexpr std::string_view constant_type_name[]{
#include "IR.def"
};

using index_t = uint32_t;
inline bool is_index_valid(index_t index) {
  return index != std::numeric_limits<index_t>::max();
}

class Module;
class Type;
struct Value;

class BasicBlock;
class Instruction;
class Constant;
class ConstantArray;
class ConstantExpr;
class Function;
class IRBuilder;

class Type {
  TypeSpecifier spec;
  TypeQualifier qual;
  void setTypeSpecifier(TypeSpecifier spec) { this->spec = spec; }
  void setTypeQualifier(TypeQualifier qual) { this->qual = qual; }
  friend IRBuilder;

public:
  std::string toString() { return ""; }
};

struct Value {
  std::variant<BasicBlock *, Instruction *, Constant *, Function *, Module *> _;
  friend IRBuilder;
  template <typename T> T *get() {
    if (std::holds_alternative<T *>(_)) {
      return std::get<T *>(_);
    }
    throw std::runtime_error("Value holds nothing");
  }
};

class BasicBlock {
  std::vector<index_t> insn;
  index_t parent;
  index_t identity;
  friend IRBuilder;

public:
  BasicBlock(index_t parent, index_t identity)
    : parent(parent), identity(identity) {}
};

class Instruction {
  OpType op;
  Type type;
  index_t parent;
  index_t identity;
  std::array<index_t, 3> arg;
  friend IRBuilder;

public:
  Instruction(OpType op, index_t parent, index_t identity, index_t arg0,
              index_t arg1, index_t arg2)
    : op(op), parent(parent), identity(identity), arg{arg0, arg1, arg2} {}
};

class Constant {
  ConstantType constant_type;
  Type type;
  index_t parent;
  index_t identity;
  friend IRBuilder;

public:
  Constant(ConstantType constant_type, index_t parent, index_t identity)
    : constant_type(constant_type), parent(parent), identity(identity) {}
};

class ConstantInteger : public Constant {
  uint64_t value;

public:
  ConstantInteger(uint64_t value, index_t parent, index_t identity)
    : Constant(ConstantType::CT_Integer, parent, identity), value(value) {}
};

class ConstantArray : public Constant {
  std::vector<index_t> element;

public:
  ConstantArray(index_t parent, index_t identity)
    : Constant(ConstantType::CT_Array, parent, identity) {}
};

class ConstantExpr : public Constant {
  OpType op;
  index_t left;
  index_t right;

public:
  ConstantExpr(OpType op, index_t parent, index_t identity, index_t left,
               index_t right)
    : Constant(ConstantType::CT_Expression, parent, identity), op(op),
      left(left), right(right) {}
};

class Function {
  Type ret;
  std::vector<Type> arg_type;
  index_t parent;
  index_t identity;
  std::vector<index_t> basic_block;
  friend IRBuilder;

public:
  Function(index_t parent, index_t identity)
    : ret(), arg_type(), parent(parent), identity(identity), basic_block() {}
};

class Module {
  std::vector<Value> pool;
  std::vector<index_t> function;
  std::vector<index_t> constant;
  std::vector<index_t> globalvalue;
  friend IRBuilder;

public:
  Module() { pool.emplace_back(Value{this}); }
};

class IRBuilder {
private:
  Module *module = nullptr;
  Function *function = nullptr;
  BasicBlock *basic_block = nullptr;
  ConstantArray *constant_array = nullptr;
  ConstantExpr *constant_expr = nullptr;
  ConstantInteger *constant_integer = nullptr;

private:
  void check_module() {
    if (module == nullptr) {
      throw std::runtime_error("Module is not initialized");
    }
  }

  void check_function() {
    if (function == nullptr) {
      throw std::runtime_error("Function is not initialized");
    }
  }

  void check_basic_block() {
    if (basic_block == nullptr) {
      throw std::runtime_error("BasicBlock is not initialized");
    }
  }

  index_t create_raw_instruction(OpType op, index_t arg0, index_t arg1,
                                 index_t arg2) {
    check_module();
    check_basic_block();
    Instruction *insn = new Instruction(op, basic_block->identity,
                                        module->pool.size(), arg0, arg1, arg2);
    basic_block->insn.push_back(insn->identity);
    module->pool.push_back(Value{insn});
    return insn->identity;
  }

public:
  IRBuilder() {}
  void createModule() { module = new Module(); }

  index_t createFunction() {
    check_module();
    function = new Function(0, module->pool.size());
    module->pool.emplace_back(Value{function});
    module->function.emplace_back(function->identity);
    return function->identity;
  }

  index_t createBasicBlock() {
    check_module();
    check_function();
    basic_block = new BasicBlock(function->identity, module->pool.size());
    module->pool.emplace_back(Value{basic_block});
    return basic_block->identity;
  }

  index_t createConstantInteger(uint64_t value) {
    check_module();
    constant_integer = new ConstantInteger(0, module->pool.size(), value);
    module->pool.emplace_back(Value{constant_integer});
    return constant_integer->identity;
  }

  index_t createConstantArray() {
    check_module();
    constant_array = new ConstantArray(0, module->pool.size());
    module->pool.emplace_back(Value{constant_array});
    return constant_array->identity;
  }

  index_t createConstantExpr(OpType op, index_t left, index_t right) {
    check_module();
    constant_expr = new ConstantExpr(op, 0, module->pool.size(), left, right);
    module->pool.emplace_back(Value{constant_expr});
    return constant_expr->identity;
  }

  ConstantArray *getConstantArray() { return constant_array; }
  ConstantExpr *getConstantExpr() { return constant_expr; }
  ConstantInteger *getConstantInteger() { return constant_integer; }
  BasicBlock *getBasicBlock() { return basic_block; }
  Function *getFunction() { return function; }
  Module *getModule() { return module; }
};
