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
struct SSAValueBase;

struct Module;
struct BasicBlock;
struct Intrinsic;
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
  uint8_t width;
  uint8_t indirect_level;
  TrivialValueVector<unsigned, 2> dimension;
};

static inline SSAType VoidType = {SSAType::PrimitiveType::Void, 0, 0, {}};
static inline SSAType IntType = {SSAType::PrimitiveType::Integer, 32, 0, {}};

struct SSAValueBase {
  SSAValueHandle parent;
  SSAValueHandle identity;
};

struct Intrinsic : public SSAValueBase {
  std::string_view name;
  SSAType type;
  std::vector<SSAValueHandle> args;
};

struct Instruction : public SSAValueBase {
  OpType op;
  SSAType type;
  std::array<SSAValueHandle, 3> arg;
};

struct BasicBlock : public SSAValueBase {
  std::vector<SSAValueHandle> insn;
};

struct ConstantInteger : public SSAValueBase {
  uint64_t value;
};

struct ConstantArray : public SSAValueBase {
  std::vector<uint64_t> array;
};

struct Function : public SSAValueBase {
  SSAType return_type;
  std::vector<std::tuple<std::string_view, Type>> args;
  std::vector<SSAValueHandle> basic_block;
  std::string_view name;
};

struct GlobalVariable : public SSAValueBase {
  SSAType type;
  std::string_view name;
};

struct SSAValue {
  std::variant<std::nullptr_t, BasicBlock *, Intrinsic *, Instruction *,
               ConstantInteger *, ConstantArray *, GlobalVariable *, Function *>
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
  inline static SSAValueHandle top_level = 0;
};

class IRHost {
private:
  SSAValuePool pool;
  std::vector<SSAValueHandle> function_table;
  std::vector<SSAValueHandle> global_value_table;
  std::vector<SSAValueHandle> constant_table;
  std::pair<Function *, SSAValueHandle> function{};
  std::pair<BasicBlock *, SSAValueHandle> basic_block{};
  std::pair<GlobalVariable *, SSAValueHandle> global_variable{};
  std::pair<Instruction *, SSAValueHandle> instruction{};

private:
  template <typename T>
  void init_parent_and_identity(
    SSAValueBase *value,
    SSAValueHandle parent = SSAValueHandle::InvalidValueHandle) {
    value->parent =
      parent == SSAValueHandle::InvalidValueHandle ? pool.top_level : parent;
    value->identity = pool.values.size();
    pool.values.emplace_back(SSAValue{static_cast<T>(value)});
  }

  void checkBasicBlock() {
    if (!basic_block.first) {
      throw std::runtime_error("BasicBlock is empty");
    }
  }

public:
  void setInsertPoint(std::pair<BasicBlock *, SSAValueHandle> pos) {
    basic_block = pos;
  }

  std::pair<Intrinsic *, SSAValueHandle> createIntrinsic(std::string_view name,
                                                         SSAType type) {
    checkBasicBlock();
    auto insn = new Intrinsic();
    init_parent_and_identity<Intrinsic *>(insn, basic_block.second);
    insn->name = name;
    insn->type = type;
    basic_block.first->insn.push_back(insn->identity);
    return {insn, insn->identity};
  }

  std::pair<Instruction *, SSAValueHandle>
  createInstruction(OpType op, SSAType type,
                    SSAValueHandle arg0 = SSAValueHandle::InvalidValueHandle,
                    SSAValueHandle arg1 = SSAValueHandle::InvalidValueHandle,
                    SSAValueHandle arg2 = SSAValueHandle::InvalidValueHandle) {
    checkBasicBlock();
    auto insn = new Instruction();
    init_parent_and_identity<Instruction *>(insn, basic_block.second);
    insn->op = op;
    insn->type = type;
    insn->arg = {arg0, arg1, arg2};
    basic_block.first->insn.push_back(insn->identity);
    return {insn, insn->identity};
  }

  std::pair<Function *, SSAValueHandle> createFunction() {
    auto function = new Function();
    init_parent_and_identity<Function *>(function);
    function_table.emplace_back(function->identity);
    return {function, function->identity};
  }

  std::pair<BasicBlock *, SSAValueHandle>
  createBasicBlock(SSAValueHandle parent) {
    auto basic_block = new BasicBlock();
    init_parent_and_identity<BasicBlock *>(basic_block, parent);
    return {basic_block, basic_block->identity};
  }

  std::pair<GlobalVariable *, SSAValueHandle> createGlobalVariable() {
    auto global_variable = new GlobalVariable();
    init_parent_and_identity<GlobalVariable *>(global_variable);
    global_value_table.emplace_back(global_variable->identity);
    return {global_variable, global_variable->identity};
  }

  std::pair<ConstantInteger *, SSAValueHandle>
  createConstantInteger(uint64_t value) {
    auto const_int = new ConstantInteger();
    init_parent_and_identity<GlobalVariable *>(const_int);
    constant_table.emplace_back(const_int->identity);
    return {const_int, const_int->identity};
  }

  std::pair<ConstantArray *, SSAValueHandle> createConstantArray() {
    auto const_arr = new ConstantArray();
    init_parent_and_identity<GlobalVariable *>(const_arr);
    constant_table.emplace_back(const_arr->identity);
    return {const_arr, const_arr->identity};
  }
};
