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
#include <unordered_set>
#include <utility>
#include <variant>
#include <vector>

#include "IR/IR.hpp"
#include "Tree/Tree.hpp"
#include "Util/Filter.hpp"
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
  [[nodiscard]] bool isValid() const {
    return id != std::numeric_limits<unsigned>::max();
  }
  static SSAValueHandle InvalidValueHandle() {
    return SSAValueHandle{std::numeric_limits<unsigned>::max()};
  }
};

template <> struct std::hash<SSAValueHandle> {
  std::size_t operator()(const SSAValueHandle &h) const noexcept {
    return std::hash<unsigned>{}(h.id);
  }
};

constexpr auto handleIsValid = [](const SSAValueHandle &handle) {
  return handle.isValid();
};

struct SSAType {
  enum class PrimitiveType : uint16_t {
    Void,
    Integer,
  } primitive_type;
  uint8_t width;
  uint8_t pointer;
  TrivialValueVector<unsigned, 2> dim;
  SSAType &reference(uint8_t n = 1) {
    pointer += n;
    return *this;
  }
  [[nodiscard]] SSAType createReference(uint8_t n = 1) const {
    SSAType ty = *this;
    ty.reference(n);
    return ty;
  }
  SSAType &dereference() {
    if (pointer != 0U)
      pointer--;
    else
      throw std::runtime_error("can't deref this");
    return *this;
  }
  [[nodiscard]] SSAType createDereference() const {
    SSAType ty = *this;
    ty.dereference();
    return ty;
  }
};

static inline const SSAType VoidType = {SSAType::PrimitiveType::Void, 0, 0, {}};
static inline const SSAType IntType = {
  SSAType::PrimitiveType::Integer, 32, 0, {}};
static inline const SSAType PointerType = {
  SSAType::PrimitiveType::Integer, 32, 1, {}};

struct SSAValue {
  SSAValueType value_type;
  SSAValueHandle parent;
  SSAValueHandle identity;
  TrivialValueVector<SSAValueHandle, 4> user;
  template <typename T> bool is() {
    return value_type == std::remove_pointer_t<T>::this_type;
  }
  template <typename T> T as() {
    if (is<T>())
      return static_cast<T>(this);
    return nullptr;
  }
  template <typename T> T as_unchecked() { return static_cast<T>(this); }
  template <typename T> T as() const {
    if (is<T>())
      return static_cast<T>(this);
    return nullptr;
  }
  template <typename T> T as_unchecked() const { return static_cast<T>(this); }
  operator SSAValueHandle() const { return identity; }
  [[nodiscard]] auto getValidUser() { return filter(user, handleIsValid); }
  void removeUser(SSAValueHandle target) {
    for (auto &user: getValidUser())
      if (user == target)
        user = SSAValueHandle::InvalidValueHandle();
  }

    void removeUser(const std::unordered_set<SSAValueHandle>& set) {
    for (auto &user: getValidUser())
      if (set.contains(user))
        user = SSAValueHandle::InvalidValueHandle();
  }
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
  TrivialValueVector<SSAValueHandle, 2> pred;
  TrivialValueVector<SSAValueHandle, 2> succ;
  unsigned extra_id;
  bool visited;
  std::unordered_set<SSAValueHandle> remove_cache;
  BasicBlock() : SSAValue{this_type} {}
  [[nodiscard]] auto getValidInstruction() {
    return filter(insn, handleIsValid);
  }
  [[nodiscard]] auto getValidPredecessor() {
    return filter(pred, handleIsValid);
  }
  [[nodiscard]] auto getValidSuccessor() { return filter(succ, handleIsValid); }
  [[nodiscard]] auto getValidInstructionFront() {
    return filter(insn, handleIsValid).front();
  }
  [[nodiscard]] auto getValidInstructionBack() {
    return reverse_filter(insn, handleIsValid).front();
  }

  void removeInstructionInFuture(SSAValueHandle handle) {
    remove_cache.insert(handle);
  }

  void removeInstruction() {
    if (remove_cache.empty())
      return;
    for (auto &&insn : insn) {
      if (remove_cache.count(insn) != 0)
        insn = SSAValueHandle::InvalidValueHandle();
    }
    remove_cache.clear();
  }
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
  [[nodiscard]] auto getValidBasicBlock() {
    return filter(basic_block, handleIsValid);
  }
  [[nodiscard]] auto getValidBasicBlockFront() {
    return filter(basic_block, handleIsValid).front();
  }
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
  Function *function{};
  BasicBlock *basic_block{};
  GlobalVariable *global_variable{};
  Instruction *instruction{};

  template <typename T>
  void init_parent_and_identity(
    SSAValue *value,
    SSAValueHandle parent = SSAValueHandle::InvalidValueHandle()) {
    value->parent = parent == SSAValueHandle::InvalidValueHandle()
                      ? SSAValuePool::top_level
                      : parent;
    value->identity = SSAValueHandle{(unsigned)pool.values.size()};
    pool.values.emplace_back(value);
  }

  void checkBasicBlock() const {
    if (basic_block == nullptr) {
      throw std::runtime_error("BasicBlock is empty");
    }
  }

public:
  void setInsertPoint(BasicBlock *pos) { basic_block = pos; }

  void replace(SSAValueHandle old_val, SSAValueHandle new_val) {
    pool.values[static_cast<unsigned>(old_val)] =
      pool.values[static_cast<unsigned>(new_val)];
  }

  [[nodiscard]] auto getInsertPoint() const { return basic_block; }

  Instruction *
  createInstruction(OpType op, SSAType type,
                    std::initializer_list<SSAValueHandle> args = {},
                    BasicBlock *bb = nullptr) {
    if (bb == nullptr)
      checkBasicBlock();
    auto *target_bb = bb != nullptr ? bb : basic_block;
    auto *insn = new Instruction();
    init_parent_and_identity<Instruction *>(insn, target_bb->identity);
    insn->op = op;
    insn->type = std::move(type);
    insn->args = args;
    target_bb->insn.push_back(insn->identity);
    return insn;
  }

  Function *createFunction() {
    auto *function = new Function();
    init_parent_and_identity<Function *>(function);
    function_table.emplace_back(function->identity);
    return function;
  }

  BasicBlock *createBasicBlock(SSAValueHandle parent) {
    auto *basic_block = new BasicBlock();
    init_parent_and_identity<BasicBlock *>(basic_block, parent);
    return basic_block;
  }

  GlobalVariable *createGlobalVariable() {
    auto *global_variable = new GlobalVariable();
    init_parent_and_identity<GlobalVariable *>(global_variable);
    global_value_table.emplace_back(global_variable->identity);
    return global_variable;
  }

  ConstantInteger *createConstantInteger(uint64_t value) {
    auto *const_int = new ConstantInteger();
    init_parent_and_identity<ConstantInteger *>(const_int);
    const_int->value = value;
    return const_int;
  }

  Argument *createArgument(SSAValueHandle parent) {
    auto *arg = new Argument();
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

  [[nodiscard]] auto getValidFunction() {
    return filter(function_table, handleIsValid);
  }

  [[nodiscard]] auto getValidGlobalVariable() {
    return filter(global_value_table, handleIsValid);
  }
};