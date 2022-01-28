#pragma once

#include <array>
#include <cstddef>
#include <cstdint>
#include <fmt/core.h>
#include <fmt/format.h>
#include <functional>
#include <initializer_list>
#include <limits>
#include <memory>
#include <stdexcept>
#include <string>
#include <string_view>
#include <unordered_map>
#include <utility>
#include <variant>
#include <vector>

#include "IR/IR.hpp"
#include "Tree/Tree.hpp"
#include "Util/TrivialValueVector.hpp"

#define SSAValueTypeDefine(x) x,
enum SSAValueType {
#include "Common/Common.def"
};

struct SSAValueHandle;
struct SSAType;
struct SSAValue;
struct SSAValue;

struct Module;
struct BasicBlock;
struct Instruction;
struct ConstantInteger;
struct GlobalVariable;
struct Function;
struct Argument;

struct IRHost;

struct SSAValueHandle {
  unsigned id;
  operator unsigned() const { return id; }
  bool isValid() const { return id != std::numeric_limits<unsigned>::max(); }
  static SSAValueHandle InvalidValueHandle() {
    return SSAValueHandle{std::numeric_limits<unsigned>::max()};
  }
};

struct SSAType {
  enum class PrimitiveType : uint16_t {
    Void,
    Integer,
  } primitive_type;
  uint8_t width;
  uint8_t indirect_level;
  TrivialValueVector<unsigned, 2> dimension;
};

static inline SSAType VoidType = {SSAType::PrimitiveType::Void, 0, 0, {}};
static inline SSAType IntType = {SSAType::PrimitiveType::Integer, 32, 0, {}};

struct SSAValue {
  SSAValueType value_type;
  SSAValueHandle parent;
  SSAValueHandle identity;
  template <typename T> bool is() {
    return value_type == std::remove_pointer_t<T>::this_type;
  }
  template <typename T> T as() {
    if (is<T>())
      return static_cast<T>(this);
    return nullptr;
  }
  template <typename T> T as_unchecked() { return static_cast<T>(this); }
  template <typename T> const T as() const {
    if (is<T>())
      return static_cast<T>(this);
    return nullptr;
  }
  template <typename T> const T as_unchecked() const {
    return static_cast<T>(this);
  }
  operator SSAValueHandle() const { return identity; }
};

#undef THIS
#define THIS(x) constexpr inline static SSAValueType this_type = x

struct Instruction : public SSAValue {
  THIS(SV_Instruction);
  OpType op;
  SSAType type;
  TrivialValueVector<SSAValueHandle, 3> args;
  Instruction() : SSAValue{this_type} {}
};

struct BasicBlock : public SSAValue {
  THIS(SV_BasicBlock);
  std::vector<SSAValueHandle> insn;
  BasicBlock() : SSAValue{this_type} {}
};

struct ConstantInteger : public SSAValue {
  THIS(SV_ConstantInteger);
  uint64_t value;
  ConstantInteger() : SSAValue{this_type} {}
};

struct Argument : public SSAValue {
  THIS(SV_Argument);
  SSAType type;
  std::string_view name;
  Argument() : SSAValue{this_type} {}
};

struct Function : public SSAValue {
  THIS(SV_Function);
  SSAType return_type;
  std::vector<SSAValueHandle> args;
  std::vector<SSAValueHandle> basic_block;
  std::string_view name;
  bool external;
  Function() : SSAValue{this_type} {}
};

struct GlobalVariable : public SSAValue {
  THIS(SV_GlobalVariable);
  SSAType type;
  std::string_view name;
  GlobalVariable() : SSAValue{this_type} {}
};

struct SSAValuePool {
  std::vector<SSAValue *> values;
  SSAValuePool() { values.emplace_back(nullptr); }
  SSAValue *operator[](SSAValueHandle n) {
    return values[static_cast<unsigned>(n)];
  }
  const SSAValue *operator[](SSAValueHandle n) const {
    return values[static_cast<unsigned>(n)];
  }
  inline static SSAValueHandle top_level{0};
};

struct IRHost {
  SSAValuePool pool;
  std::vector<SSAValueHandle> function_table;
  std::vector<SSAValueHandle> global_value_table;
  std::vector<SSAValueHandle> constant_table;
  Function *function{};
  BasicBlock *basic_block{};
  GlobalVariable *global_variable{};
  Instruction *instruction{};

  template <typename T>
  void init_parent_and_identity(
    SSAValue *value,
    SSAValueHandle parent = SSAValueHandle::InvalidValueHandle()) {
    value->parent =
      parent == SSAValueHandle::InvalidValueHandle() ? pool.top_level : parent;
    value->identity = SSAValueHandle{(unsigned)pool.values.size()};
    pool.values.emplace_back(value);
  }

  void checkBasicBlock() {
    if (!basic_block) {
      throw std::runtime_error("BasicBlock is empty");
    }
  }

public:
  void setInsertPoint(BasicBlock *pos) { basic_block = pos; }

  auto getInsertPoint() { return basic_block; }

  Instruction *createInstruction(OpType op, SSAType type,
                                 std::initializer_list<SSAValueHandle> args = {}) {
    checkBasicBlock();
    auto insn = new Instruction();
    init_parent_and_identity<Instruction *>(insn, basic_block->identity);
    insn->op = op;
    insn->type = type;
    insn->args = args;
    basic_block->insn.push_back(insn->identity);
    return insn;
  }

  Function *createFunction() {
    auto function = new Function();
    init_parent_and_identity<Function *>(function);
    function_table.emplace_back(function->identity);
    return function;
  }

  BasicBlock *createBasicBlock(SSAValueHandle parent) {
    auto basic_block = new BasicBlock();
    init_parent_and_identity<BasicBlock *>(basic_block, parent);
    return basic_block;
  }

  GlobalVariable *createGlobalVariable() {
    auto global_variable = new GlobalVariable();
    init_parent_and_identity<GlobalVariable *>(global_variable);
    global_value_table.emplace_back(global_variable->identity);
    return global_variable;
  }

  ConstantInteger *createConstantInteger(uint64_t value) {
    auto const_int = new ConstantInteger();
    init_parent_and_identity<ConstantInteger *>(const_int);
    const_int->value = value;
    constant_table.emplace_back(const_int->identity);
    return const_int;
  }

  Argument *createArgument(SSAValueHandle parent) {
    auto arg = new Argument();
    init_parent_and_identity<Argument *>(arg, parent);
    return arg;
  }

  SSAValue &operator[](SSAValueHandle n) {
    if (n.isValid() && n.id < pool.values.size()) {
      return *pool[n];
    }
    throw std::runtime_error("SSAValueHandle is invalid");
  }

  SSAValueHandle Zero = *createConstantInteger(0);
};