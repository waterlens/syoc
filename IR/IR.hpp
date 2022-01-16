#pragma once

#include <fmt/core.h>
#include <array>
#include <cstddef>
#include <cstdint>
#include <fmt/format.h>
#include <functional>
#include <limits>
#include <memory>
#include <stdexcept>
#include <stdint.h>
#include <string>
#include <string_view>
#include <unordered_map>
#include <utility>
#include <variant>
#include <vector>


#define OpcodeDefine(x, n) x,
enum OpType {
#include "Common/Common.def"
};

#define OpcodeDefine(x, n) n,
inline constexpr std::string_view op_name[]{
#include "Common/Common.def"
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
class ConstantArray;
class ConstantExpr;
class ConstantInteger;
class GlobalVariable;
class Function;
class IRBuilder;

class Type {
  friend IRBuilder;
};

class ConstantInteger;


class BasicBlock {
  std::vector<ValueHandle> insn;
  ValueHandle parent;
  ValueHandle identity;
  friend IRBuilder;

public:
  BasicBlock(ValueHandle parent, ValueHandle identity)
    : parent(parent), identity(identity) {}
  auto &refInstruction() { return insn; }
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
  /*std::string toString() {
    static std::string arg_str;
    arg_str.clear();
    for (auto &a : arg)
      if (is_handle_valid(a))
        arg_str.append(fmt::format("%{}, ", a));
    if (arg_str.ends_with(", "))
      arg_str.pop_back(), arg_str.pop_back();
    return fmt::format("%{} = {} {} {}", identity, op_name[op], type.toString(),
                       arg_str);
  }*/
  Instruction(OpType op, Type type, ValueHandle parent, ValueHandle identity,
              ValueHandle arg0, ValueHandle arg1, ValueHandle arg2)
    : op(op), type(type), parent(parent),
      identity(identity), arg{arg0, arg1, arg2} {}
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

class ConstantInteger {
  uint64_t value;
  friend IRBuilder;

public:
  ConstantInteger(uint64_t value) : value(value) {}
  uint64_t &refValue() { return value; }
};

class ConstantArray {
  ValueHandle parent;
  ValueHandle identity;
  std::vector<ValueHandle> element;
  friend IRBuilder;

public:
  ConstantArray(ValueHandle parent, ValueHandle identity)
    : parent(parent), identity(identity) {}
  auto &refElement() { return element; }
  std::string toString() {
    static std::string buffer;
    buffer.clear();
    buffer.append(fmt::format("%{} = ", identity));
    buffer.append("[");
    for (auto &e : element)
      buffer.append(fmt::format("%{}, ", e));
    if (buffer.ends_with(", "))
      buffer.pop_back(), buffer.pop_back();
    buffer.append("]");
    return buffer;
  }
};

class ConstantExpr {
  ValueHandle parent;
  ValueHandle identity;
  OpType op;
  ValueHandle left;
  ValueHandle right;
  friend IRBuilder;

public:
  ConstantExpr(OpType op, ValueHandle parent, ValueHandle identity,
               ValueHandle left, ValueHandle right)
    : parent(parent), identity(identity), op(op), left(left), right(right) {}
  std::string toString() {
    static std::string arg_str;
    arg_str.clear();
    if (is_handle_valid(left))
      arg_str.append(fmt::format("%{}", left));
    if (is_handle_valid(right))
      arg_str.append(fmt::format(", %{}", right));
    return fmt::format("%{} = {} {}", identity, op_name[op], arg_str);
  }
};

class Function {
  Type ret;
  std::vector<std::tuple<std::string_view, Type>> args;
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


struct Value {
  std::variant<BasicBlock *, Instruction *, GlobalVariable *, ConstantInteger,
               ConstantArray *, ConstantExpr *, Function *, Module *>
    _;
  friend IRBuilder;
  template <typename T> T get() {
    if (std::holds_alternative<T>(_)) {
      return std::get<T>(_);
    }
    throw std::runtime_error("Value not holds T");
  }
  template <typename T> T get_if() { return std::get_if<T>(_); }
};

class Module {
  std::vector<Value> pool;
  std::vector<ValueHandle> global_function_table;
  std::vector<ValueHandle> global_value_table;
  std::vector<ValueHandle> global_constant_table;
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
  create_raw_instruction(OpType op, Type type, ValueHandle arg0,
                         ValueHandle arg1, ValueHandle arg2) {
    check_module();
    check_basic_block();
    Instruction *insn = new Instruction(op, type, basic_block->identity,
                                        module->pool.size(), arg0, arg1, arg2);
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

  ValueHandle
  createConstantInteger(uint64_t value) {
    check_module();
    auto identity = module->pool.size();
    module->pool.emplace_back(Value{ConstantInteger{value}});
    return identity;
  }

  std::tuple<ValueHandle, ConstantArray *> createConstantArray() {
    check_module();
    constant_array = new ConstantArray(module_handle, module->pool.size());
    module->pool.emplace_back(Value{constant_array});
    module->global_constant_table.push_back(constant_array->identity);
    return {constant_array->identity, constant_array};
  }

  std::tuple<ValueHandle, ConstantExpr *>
  createConstantExpr(OpType op, ValueHandle left, ValueHandle right) {
    check_module();
    constant_expr =
      new ConstantExpr(op, module_handle, module->pool.size(), left, right);
    module->pool.emplace_back(Value{constant_expr});
    module->global_constant_table.push_back(constant_expr->identity);
    return {constant_expr->identity, constant_expr};
  }

  std::tuple<ValueHandle, Instruction *>
  createInstruction(OpType op, ValueHandle v1,
                    ValueHandle v2 = invalid_value_handle,
                    ValueHandle v3 = invalid_value_handle) {
    return create_raw_instruction(op, {}, v1, v2, v3);
  }

  std::tuple<ValueHandle, Instruction *>
  createInstruction(OpType op, Type type, ValueHandle v1,
                    ValueHandle v2 = invalid_value_handle,
                    ValueHandle v3 = invalid_value_handle) {
    return create_raw_instruction(op, type, v1, v2, v3);
  }

  GlobalVariable *getGlobalVariable() { return global_variable; }
  ConstantArray *getConstantArray() { return constant_array; }
  ConstantExpr *getConstantExpr() { return constant_expr; }
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

  void dumpGraph();
  void dumpText();
};
