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

#include "Tree/Tree.hpp"
#include "Util/Filter.hpp"
#include "Util/TrivialValueList.hpp"
#include "Util/TrivialValueVector.hpp"

namespace YIR {

#define SSAValueTypeDefine(x) x,
enum ClassType {
#include "Common/Common.def"
};

struct Type;
struct Value;
struct Module;
struct BasicBlock;
struct Instruction;
struct ConstantInteger;
struct GlobalVariable;
struct Function;
struct Argument;

struct Type {
  enum class PrimitiveType : uint16_t {
    Void,
    Integer,
  } primitive_type;
  uint8_t width;
  uint8_t pointer;
  Type &reference(uint8_t n = 1) {
    pointer += n;
    return *this;
  }
  [[nodiscard]] Type createReference(uint8_t n = 1) const {
    Type ty = *this;
    ty.reference(n);
    return ty;
  }
  Type &dereference() {
    if (pointer != 0U)
      pointer--;
    else
      throw std::runtime_error("can't deref this");
    return *this;
  }
  [[nodiscard]] Type createDereference() const {
    Type ty = *this;
    ty.dereference();
    return ty;
  }
};

struct PredefinedType {
  static inline const Type Void = {Type::PrimitiveType::Void, 0, 0};
  static inline const Type Int32 = {Type::PrimitiveType::Integer, 32, 0};
  static inline const Type IntPtr = {Type::PrimitiveType::Integer, 32, 1};
};

struct UseEdge : public ListNode<UseEdge> {
  Value *from;
  Value *to;
  UseEdge(Value *from, Value *to);
  ~UseEdge();
};

struct BasicBlockEdge : public ListNode<BasicBlockEdge> {
  BasicBlock *from;
  BasicBlock *to;
  BasicBlockEdge(Value *from, Value *to) {}
  ~BasicBlockEdge() {}
};

struct Value {
protected:
  ListIterator<UseEdge> edge;
  Value *parent;
  ClassType class_type;
  unsigned identity = std::numeric_limits<unsigned>::max();

public:
  unsigned &getIdentity() { return identity; }
  Value *&getParent() { return parent; }
  Value(ClassType t) : class_type(t) {}
  template <typename T> bool is() {
    return class_type == std::remove_pointer_t<T>::this_type;
  }
  template <typename T> T as() {
    if (is<T>())
      return static_cast<T>(this);
    return nullptr;
  }
  template <typename T> T as() const { return as<T>(); }
  auto &getEdge() { return edge; }
  void removeEdge(UseEdge *edge) {
    if (edge == getEdge().base())
      ++getEdge();
    edge->remove_from_list();
  }
  void addEdge(UseEdge *edge) {
    if (getEdge().base() != nullptr) {
      getEdge()->insert_before(edge);
      --getEdge();
    } else
      getEdge() = edge;
  }
  [[nodiscard]] auto getImmutableEdges() const { return edge; }
};

#undef THIS
#define THIS(x) constexpr inline static ClassType this_type = x

struct Instruction : public Value, public ListNode<Instruction> {
  THIS(SV_Instruction);
  OpType op;
  Type type;
  std::vector<UseEdge> input;
  Instruction() : Value{this_type} {}
  static Instruction *create(OpType op, Type type,
                             std::initializer_list<Value *> inputs = {},
                             BasicBlock *bb = nullptr);
  [[nodiscard]] const auto &getInput() const { return input; }
  auto &getInput() { return input; }
  struct InputProxy {
    UseEdge *edge;
    Value *user;
    InputProxy &operator=(Value *v) {
      assert(edge != nullptr);
      if (edge->from != nullptr)
        edge->remove_from_list();
      edge->from = v;
      edge->to = user;
      if (v != nullptr)
        v->addEdge(edge);
      return *this;
    }
  };
  InputProxy operator[](size_t i) {
    if (i >= input.size())
      throw std::runtime_error("out of range");
    return InputProxy{&input[i], this};
  }
  InputProxy getLastInput() { return InputProxy{&input.back(), this}; }
  void addInput(Value *value) {
    input.emplace_back(nullptr, this);
    getLastInput() = value;
  }
};

struct BasicBlock : public Value, public ListNode<BasicBlock> {
  THIS(SV_BasicBlock);
  List<Instruction> insn;
  std::vector<BasicBlockEdge> pred;
  ListIterator<BasicBlockEdge> succ;
  unsigned order;
  bool visited;
  BasicBlock() : Value{this_type} {}
  static BasicBlock *create(Function *f = nullptr);
  [[nodiscard]] bool hasPredecessor() const { return !pred.empty(); }
  [[nodiscard]] bool hasSuccessor() const { return succ.base() != nullptr; }
  [[nodiscard]] auto &getPredecessor() { return pred; }
  [[nodiscard]] auto &getSuccessorBegin() { return succ; }
  [[nodiscard]] auto &getInstruction() { return insn; }
  [[nodiscard]] auto begin() const { return insn.cbegin(); }
  [[nodiscard]] auto end() const { return insn.cend(); }
  [[nodiscard]] auto begin() { return insn.begin(); }
  [[nodiscard]] auto end() { return insn.end(); }
  void linkByBranch(Value *cond, BasicBlock *true_bb, BasicBlock *false_bb) {
    Instruction::create(OP_Branch, PredefinedType::Void,
                        {cond, true_bb, false_bb}, this);
  }
  void linkByJump(BasicBlock *next_bb) {
    Instruction::create(OP_Jump, PredefinedType::Void, {next_bb}, this);
  }
};

struct ConstantInteger : public Value {
  THIS(SV_ConstantInteger);
  uint64_t value;
  ConstantInteger() : Value{this_type} {}
  static ConstantInteger *create(uint64_t value) {
    auto *p = new ConstantInteger();
    p->value = value;
    p->parent = nullptr;
    return p;
  }
};

struct Argument : public Value {
  THIS(SV_Argument);
  Type type;
  std::string_view name;
  Argument() : Value{this_type} {}
  static Argument *create(Type type, std::string_view name, Function *f);
  auto &getType() { return type; }
  [[nodiscard]] auto getName() const { return name; }
};

struct Function : public Value {
  THIS(SV_Function);
  Type return_type;
  std::vector<Argument *> arg;
  List<BasicBlock> block;
  std::string_view name;
  bool external;
  Function() : Value{this_type} {}
  bool &refExternal() { return external; }
  static Function *create(Type type, std::string_view name,
                          Module *m = nullptr);
  void addBasicBlock(BasicBlock *bb) {
    block.push_back(bb);
    bb->getParent() = this;
  }
};

struct GlobalVariable : public Value {
  THIS(SV_GlobalVariable);
  Type type;
  std::string_view name;
  unsigned capacity;
  GlobalVariable() : Value{this_type} {}
  static GlobalVariable *create(Type type, std::string_view name,
                                unsigned capacity, Module *m = nullptr);
};

struct Module : public Value {
  THIS(SV_Module);
  std::vector<Function *> func;
  std::vector<GlobalVariable *> global;
  Module() : Value{this_type} {}
};

struct IRHost {
  Module *root;
  BasicBlock *basic_block;
  IRHost() { root = new Module(); }
  [[nodiscard]] Module *getModule() const { return root; }
  void setInsertPoint(BasicBlock *bb) { basic_block = bb; }
  [[nodiscard]] BasicBlock *getInsertPoint() const { return basic_block; }
  void insertInstruction(Instruction *insn) const {
    if (basic_block == nullptr)
      throw std::runtime_error("basic block is not specified");
    basic_block->getInstruction().push_back(insn);
  }
  auto *createInstruction(OpType op, Type type,
                          std::initializer_list<Value *> inputs = {},
                          BasicBlock *bb = nullptr) const {
    if (bb != nullptr)
      return Instruction::create(op, type, inputs, bb);
    if (basic_block == nullptr)
      throw std::runtime_error("basic block is not specified");
    return Instruction::create(op, type, inputs, basic_block);
  }
};

} // namespace YIR