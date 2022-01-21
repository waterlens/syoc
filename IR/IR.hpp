#pragma once

#include <array>
#include <cstddef>
#include <cstdint>
#include <fmt/core.h>
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

#include "IR/IR.hpp"
#include "Tree/Tree.hpp"
#include "Util/TrivialValueVector.hpp"

struct SSAValueHandle;
struct SSAType;
struct SSAValue;

struct Module;
struct BasicBlock;
struct Instruction;
struct ConstantInteger;
struct ConstantArray;
struct GlobalVariable;
struct Function;

class IRHost;

struct SSAValueHandle {
  unsigned id;
  inline static unsigned InvalidValueHandle =
    std::numeric_limits<unsigned>::max();
  SSAValueHandle(const SSAValueHandle &handle) : id(handle.id) {}
  SSAValueHandle(SSAValueHandle &&handle) : id(handle.id) {}
  SSAValueHandle(unsigned id) : id(id) {}
  SSAValueHandle() : id(InvalidValueHandle) {}
  ~SSAValueHandle() {}
  SSAValueHandle &operator=(const SSAValueHandle &handle) {
    id = handle.id;
    return *this;
  }
  operator unsigned() const { return id; }
  bool isValid() const { return id != InvalidValueHandle; }
};

struct SSAType {
  enum class PrimitiveType : uint16_t {
    Void,
    Integer,
  } primitive_type;
  uint16_t width;
  TrivialValueVector<unsigned> dimension;
};

struct Instruction {
  SSAValueHandle parent;
  SSAValueHandle identity;
  OpType op;
  SSAType type;
  std::array<SSAValueHandle, 3> arg;
};

struct BasicBlock {
  SSAValueHandle parent;
  SSAValueHandle identity;
  std::vector<SSAValueHandle> insn;
};

struct ConstantInteger {
  SSAValueHandle parent;
  SSAValueHandle identity;
  uint64_t value;
};

struct ConstantArray {
  SSAValueHandle parent;
  SSAValueHandle identity;
  std::vector<uint64_t> array;
};

struct Function {
  SSAValueHandle parent;
  SSAValueHandle identity;
  SSAType return_type;
  std::vector<std::tuple<std::string_view, Type>> args;
  std::vector<SSAValueHandle> basic_block;
  std::string_view name;
};

struct GlobalVariable {
  SSAValueHandle parent;
  SSAValueHandle identity;
  SSAType type;
  SSAValueHandle initializer;
  std::string_view name;
};

struct SSAValue {
  std::variant<std::nullptr_t, BasicBlock *, Instruction *, ConstantInteger *,
               ConstantArray *, GlobalVariable *, Function *>
    _;
  template <typename T> T get() {
    if (std::holds_alternative<T>(_)) {
      return std::get<T>(_);
    }
    throw std::runtime_error("SSAValue doesn't hold T");
  }
  template <typename T> T get_if() { return std::get_if<T>(_); }
};

struct SSAValuePool {
  std::vector<SSAValue> values;
  SSAValuePool() { values.emplace_back(SSAValue{nullptr}); }
  SSAValue &operator[](SSAValueHandle n) {
    return values[static_cast<unsigned>(n)];
  }
  inline static SSAValueHandle top = 0;
};

class IRHost {
private:
  SSAValuePool pool;
  std::vector<SSAValueHandle> function_table;
  std::vector<SSAValueHandle> value_table;
  std::vector<SSAValueHandle> constant_table;
  std::pair<Function *, SSAValueHandle> function{};
  std::pair<BasicBlock *, SSAValueHandle> basic_block{};
  std::pair<GlobalVariable *, SSAValueHandle> global_variable{};
  std::pair<Instruction *, SSAValueHandle> instruction{};

private:
  void check_function() {
    if (function.first == nullptr)
      throw std::runtime_error("Function is not initialized");
  }

  void check_basic_block() {
    if (basic_block.first == nullptr)
      throw std::runtime_error("BasicBlock is not initialized");
  }

public:
  std::pair<Function *, SSAValueHandle> createFunction() {
    auto function = new Function();
    function->parent = pool.top;
    function->identity = pool.values.size();
    pool.values.emplace_back(SSAValue{function});
    return {function, function->identity};
  }

  std::pair<BasicBlock *, SSAValueHandle> createBasicBlock() {
    check_function();
    auto basic_block = new BasicBlock();
    basic_block->parent = pool.top;
    basic_block->identity = pool.values.size();
    pool.values.emplace_back(SSAValue{basic_block});
    return {basic_block, basic_block->identity};
  }

  std::pair<GlobalVariable *, SSAValueHandle> createGlobalVariable() {
    auto global_variable = new GlobalVariable();
    global_variable->parent = pool.top;
    global_variable->identity = pool.values.size();
    pool.values.emplace_back(SSAValue{global_variable});
    return {global_variable, global_variable->identity};
  }

  std::pair<ConstantInteger *, SSAValueHandle>
  createConstantInteger(uint64_t value) {
    auto const_int = new ConstantInteger();
    const_int->parent = pool.top;
    const_int->identity = pool.values.size();
    pool.values.emplace_back(SSAValue{const_int});
    return {const_int, const_int->identity};
  }

  std::pair<ConstantArray *, SSAValueHandle> createConstantArray() {
    auto const_arr = new ConstantArray();
    const_arr->parent = pool.top;
    const_arr->identity = pool.values.size();
    pool.values.emplace_back(SSAValue{const_arr});
    return {const_arr, const_arr->identity};
  }
};
