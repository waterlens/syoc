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

using ValueHandle = uint32_t;
inline constexpr ValueHandle invalid_value_handle =
  std::numeric_limits<ValueHandle>::max();
inline bool is_handle_valid(ValueHandle handle) {
  return handle != invalid_value_handle;
}

class Module;
class Type;
struct Value;

class BasicBlock;
class Instruction;
class Constant;
class ConstantArray;
class ConstantExpr;
class ConstantInteger;
class GlobalVariable;
class Function;
class IRBuilder;

class Type {
  TypeSpecifier spec;
  TypeQualifier qual;
  std::vector<uint32_t> dim;
  friend IRBuilder;

public:
  Type(TypeSpecifier spec, TypeQualifier qual, std::vector<uint32_t> dim)
    : spec(spec), qual(qual), dim(dim) {}
  Type() : spec(), qual(), dim() {}
  std::string toString() { return ""; }
  TypeSpecifier &refTypeSpecifier() { return spec; }
  TypeQualifier &refTypeQualifier() { return qual; }
  std::vector<uint32_t> &refDimension() { return dim; }
};

struct Value {
  std::variant<BasicBlock *, Instruction *, GlobalVariable *, Constant *,
               Function *, Module *>
    _;
  friend IRBuilder;
  template <typename T> T *get() {
    if (std::holds_alternative<T *>(_)) {
      return std::get<T *>(_);
    }
    throw std::runtime_error("Value not holds T");
  }
};

class BasicBlock {
  std::vector<ValueHandle> insn;
  ValueHandle parent;
  ValueHandle identity;
  friend IRBuilder;

public:
  BasicBlock(ValueHandle parent, ValueHandle identity)
    : parent(parent), identity(identity) {}
};

class Instruction {
  OpType op;
  Type type;
  ValueHandle parent;
  ValueHandle identity;
  std::array<ValueHandle, 3> arg;
  friend IRBuilder;

public:
  auto &refType() { return type; }
  Instruction(OpType op, ValueHandle parent, ValueHandle identity,
              ValueHandle arg0, ValueHandle arg1, ValueHandle arg2)
    : op(op), parent(parent), identity(identity), arg{arg0, arg1, arg2} {}
};

class GlobalVariable {
  Type type;
  ValueHandle parent;
  ValueHandle identity;
  ValueHandle initializer;
  std::string_view name;
  friend IRBuilder;

public:
  GlobalVariable(ValueHandle parent, ValueHandle identity,
                 ValueHandle initializer)
    : parent(parent), identity(identity), initializer(initializer) {}
  ValueHandle &refInitializer() { return initializer; }
  Type &refType() { return type; }
  std::string_view &refName() { return name; }
};

class ConstantArray;
class ConstantExpr;
class ConstantInteger;

class Constant {
  ConstantType constant_type;
  Type type;
  ValueHandle parent;
  ValueHandle identity;
  std::string_view name;
  friend IRBuilder;

public:
  Constant(ConstantType constant_type, Type type, ValueHandle parent,
           ValueHandle identity, std::string_view name)
    : constant_type(constant_type), type(type), parent(parent),
      identity(identity), name(name) {}
  auto &refName() { return name; }
  auto &refType() { return type; }
  auto &refConstantType() { return constant_type; }
  template <typename T> T cast();
};

class ConstantInteger : public Constant {
  uint64_t value;

public:
  ConstantInteger(uint64_t value, ValueHandle parent, ValueHandle identity,
                  std::string_view name = "(integer)")
    : Constant(ConstantType::CT_Integer, Type{TS_Int, TQ_Const, {}}, parent,
               identity, name),
      value(value) {}
};

class ConstantArray : public Constant {
  std::vector<ValueHandle> element;

public:
  ConstantArray(ValueHandle parent, ValueHandle identity,
                std::string_view name = "(constant array)")
    : Constant(ConstantType::CT_Array, {}, parent, identity, name) {}
  auto &refElement() { return element; }
};

class ConstantExpr : public Constant {
  OpType op;
  ValueHandle left;
  ValueHandle right;

public:
  ConstantExpr(OpType op, ValueHandle parent, ValueHandle identity,
               ValueHandle left, ValueHandle right,
               std::string_view name = "(constant expression")
    : Constant(ConstantType::CT_Expression, Type{TS_Int, TQ_Const, {}}, parent,
               identity, name),
      op(op), left(left), right(right) {}
};

class Function {
  Type ret;
  std::vector<ValueHandle> args;
  ValueHandle parent;
  ValueHandle identity;
  std::vector<ValueHandle> basic_block;
  std::string_view name;
  friend IRBuilder;

public:
  Function(ValueHandle parent, ValueHandle identity)
    : ret(), args(), parent(parent), identity(identity), basic_block(), name() {
  }
  auto &refName() { return name; }
  auto &refReturnType() { return ret; }
  auto &refArgumentList() { return args; }
  auto &refBasicBlock() { return basic_block; }
};

class Module {
  std::vector<Value> pool;
  std::vector<ValueHandle> global_function_table;
  std::vector<ValueHandle> global_value_table;
  friend IRBuilder;

public:
  Module() { pool.emplace_back(Value{this}); }
  template <typename T> T handle_cast(ValueHandle handle) {
    return pool.at(handle).get<T>();
  }
};

class IRBuilder {
private:
  Module *module = nullptr;
  Function *function = nullptr;
  BasicBlock *basic_block = nullptr;
  GlobalVariable *global_variable = nullptr;
  ConstantArray *constant_array = nullptr;
  ConstantExpr *constant_expr = nullptr;
  ConstantInteger *constant_integer = nullptr;
  Instruction *instruction = nullptr;
  inline static ValueHandle module_handle = 0;

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

  std::tuple<ValueHandle, Instruction *>
  create_raw_instruction(OpType op, ValueHandle arg0, ValueHandle arg1,
                         ValueHandle arg2) {
    check_module();
    check_basic_block();
    Instruction *insn = new Instruction(op, basic_block->identity,
                                        module->pool.size(), arg0, arg1, arg2);
    basic_block->insn.push_back(insn->identity);
    module->pool.push_back(Value{insn});
    return {insn->identity, insn};
  }

public:
  IRBuilder() {}
  void createModule() {
    if (module)
      throw std::runtime_error("Module already exists");
    module = new Module();
  }

  std::tuple<ValueHandle, Function *> createFunction() {
    check_module();
    function = new Function(module_handle, module->pool.size());
    module->pool.emplace_back(Value{function});
    return {function->identity, function};
  }

  std::tuple<ValueHandle, BasicBlock *> createBasicBlock() {
    check_module();
    check_function();
    basic_block = new BasicBlock(function->identity, module->pool.size());
    module->pool.emplace_back(Value{basic_block});
    return {basic_block->identity, basic_block};
  }

  std::tuple<ValueHandle, GlobalVariable *> createGlobalVariable() {
    check_module();
    global_variable = new GlobalVariable(module_handle, module->pool.size(),
                                         invalid_value_handle);
    module->pool.emplace_back(Value{global_variable});
    return {global_variable->identity, global_variable};
  }

  std::tuple<ValueHandle, ConstantInteger *>
  createConstantInteger(uint64_t value) {
    check_module();
    constant_integer =
      new ConstantInteger(module_handle, module->pool.size(), value);
    module->pool.emplace_back(Value{constant_integer});
    return {constant_integer->identity, constant_integer};
  }

  std::tuple<ValueHandle, ConstantArray *> createConstantArray() {
    check_module();
    constant_array = new ConstantArray(module_handle, module->pool.size());
    module->pool.emplace_back(Value{constant_array});
    return {constant_array->identity, constant_array};
  }

  std::tuple<ValueHandle, ConstantExpr *>
  createConstantExpr(OpType op, ValueHandle left, ValueHandle right) {
    check_module();
    constant_expr = new ConstantExpr(op, module_handle,
                                     module->pool.size(), left, right);
    module->pool.emplace_back(Value{constant_expr});
    return {constant_expr->identity, constant_expr};
  }

  std::tuple<ValueHandle, Instruction *>
  createInstruction(OpType op, ValueHandle left, ValueHandle right) {
    return create_raw_instruction(op, left, right, invalid_value_handle);
  }

  GlobalVariable *getGlobalVariable() { return global_variable; }
  ConstantArray *getConstantArray() { return constant_array; }
  ConstantExpr *getConstantExpr() { return constant_expr; }
  ConstantInteger *getConstantInteger() { return constant_integer; }
  BasicBlock *getBasicBlock() { return basic_block; }
  Function *getFunction() { return function; }
  Module *getModule() { return module; }

  void addGlobalValue(ValueHandle handle) {
    check_module();
    module->global_value_table.push_back(handle);
  }

  void addFunction(ValueHandle handle) {
    check_module();
    module->global_function_table.push_back(handle);
  }

  void dumpAll();
};
