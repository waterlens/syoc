#pragma once

#include <array>
#include <cstddef>
#include <cstdint>
#include <fmt/core.h>
#include <fmt/format.h>
#include <functional>
#include <initializer_list>
#include <limits>
#include <stdexcept>
#include <string>
#include <string_view>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#include "Tree/Tree.hpp"
#include "Util/Filter.hpp"
#include "Util/List.hpp"
#include "Util/TrivialValueVector.hpp"

namespace SyOC {

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
    Float,
  } primitive_type;
  uint8_t width;
  uint8_t pointer;

  [[nodiscard]] inline bool isInt() const { return primitive_type == PrimitiveType::Integer; }
  [[nodiscard]] inline bool isFloat() const { return primitive_type == PrimitiveType::Float; }
  [[nodiscard]] inline bool isVoid() const { return primitive_type == PrimitiveType::Void; }
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
  static constexpr Type Void = {Type::PrimitiveType::Void, 0, 0};
  static constexpr Type VoidPtr = {Type::PrimitiveType::Void, 32, 1};
  static constexpr Type Int32 = {Type::PrimitiveType::Integer, 32, 0};
  static constexpr Type IntPtr = {Type::PrimitiveType::Integer, 32, 1};
  static constexpr Type Float = {Type::PrimitiveType::Float, 32, 0};
  static constexpr Type FloatPtr = { Type::PrimitiveType::Float, 32, 1};
};

struct UseEdge final : public ListNode<UseEdge> {
  Value *from;
  Value *to;
  UseEdge() = delete;
  UseEdge(const UseEdge &edge) = delete;
  UseEdge(UseEdge &&edge) noexcept;
  UseEdge(Value *from, Value *to);
  ~UseEdge() final;
  void associate(Value *from);
  UseEdge &operator=(Value *from);
  UseEdge &operator=(UseEdge &&edge) noexcept;
};

struct BasicBlockEdge final : public ListNode<BasicBlockEdge> {
  BasicBlock *from;
  BasicBlock *to;
  BasicBlockEdge() = delete;
  BasicBlockEdge(const BasicBlockEdge &edge) = delete;
  BasicBlockEdge(BasicBlockEdge &&edge) noexcept;
  BasicBlockEdge(BasicBlock *from, BasicBlock *to);
  ~BasicBlockEdge() final;
  void associate(BasicBlock *from);
  BasicBlockEdge &operator=(BasicBlock *from);
  BasicBlockEdge &operator=(BasicBlockEdge &&edge) noexcept;
};

struct Value {
protected:
  ListIterator<UseEdge> edge;
  Value *parent;
  ClassType class_type;
  unsigned identity = std::numeric_limits<unsigned>::max();

public:
  unsigned &getIdentity() { return identity; }
  Value *&refParent() { return parent; }
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
  [[nodiscard]] bool hasNoEdge() const { return edge.reach_end(); }
  auto &getEdge() { return edge; }
  void removeEdge(UseEdge *edge) {
    if (edge == getEdge().base())
      ++getEdge();
    edge->remove_from_list();
  }
  void addEdge(UseEdge *edge) {
    if (!hasNoEdge()) {
      getEdge()->insert_before(edge);
      --getEdge();
    } else
      getEdge() = edge;
  }
  [[nodiscard]] auto getEdgeHead() const { return edge; }

  struct UserView {
    const ListIterator<UseEdge> &edge_head;
    ListIterator<UseEdge> begin() { return edge_head; }
    ListIterator<UseEdge> end() {
      return edge_head.null_end(); // NOLINT
    }
  };

  [[nodiscard]] size_t getNumOfEdges() const {
    size_t n = 0;
    for (auto use_iter = getEdgeHead(); !use_iter.reach_end(); ++use_iter) ++n;
    return n;
  }

  void replaceAllUsesWith(Value *new_value) const {
    static std::vector<decltype(edge)> uses;
    uses.clear();
    for (auto use_iter = getEdgeHead(); !use_iter.reach_end(); ++use_iter)
      uses.push_back(use_iter);
    for (auto &use : uses) use->associate(new_value);
  }
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
  static Instruction *create(OpType op, Type type,
                             const std::vector<Value *> &Vec,
                             BasicBlock *bb = nullptr);
  [[nodiscard]] const auto &getInput() const { return input; }
  auto &getInput() { return input; }
  UseEdge &getInput(size_t num) { return input[num]; }
  Value *getOperand(size_t num) { return input[num].from; }
  [[nodiscard]] auto getNumOperands() const { return input.size(); }

  static inline auto addEdgeAction = [](UseEdge *edge, Value *from, Value *) {
    from->addEdge(edge);
  };

  void addInput(Value *value) {
    input.emplace_back(nullptr, this);
    input.back() = value;
  }

  [[nodiscard]] bool isMemoryAccessInst() const {
      return (op == OP_Load || op == OP_Store);
  }

  [[nodiscard]] bool isCompareInst() const {
    return (op == OP_Gt || op == OP_Ge || op == OP_Lt || op == OP_Le
            || op == OP_Eq || op == OP_Ne);
  }

  [[nodiscard]] bool isControlInstruction() const {
    return op == OP_Jump || op == OP_Branch || op == OP_Return;
  }

  [[nodiscard]] bool isDefinitionInstruction() const {
    return op == OP_Store || op == OP_Memset0;
  }

  [[nodiscard]] bool safeEliminative() const {
    return !(isControlInstruction() || isDefinitionInstruction() ||
             op == OP_Call);
  }
};

struct BasicBlock final : public Value, public ListNode<BasicBlock> {
  THIS(SV_BasicBlock);

private:
  List<Instruction> insn;
  std::vector<BasicBlockEdge> pred;
  ListIterator<BasicBlockEdge> succ;
  unsigned order;
  bool visited;

public:
  BasicBlock() : Value{this_type} {}
  ~BasicBlock() final;
  static BasicBlock *create(Function *f = nullptr);
  [[nodiscard]] auto begin() const { return insn.cbegin(); }
  [[nodiscard]] auto end() const { return insn.cend(); }
  [[nodiscard]] auto begin() { return insn.begin(); }
  [[nodiscard]] auto end() { return insn.end(); }
  auto &getInstruction() { return insn; }
  auto &getPredecessor() { return pred; }
  [[nodiscard]] auto getNumPredecessor() const { return pred.size(); }
  auto &getSuccessorHead() { return succ; }
  void removeSuccessor(BasicBlockEdge *edge) {
    if (edge == getSuccessorHead().base())
      ++getSuccessorHead();
    edge->remove_from_list();
  }
  void addSuccessor(BasicBlockEdge *edge) {
    if (getSuccessorHead().base() != nullptr) {
      getSuccessorHead()->insert_before(edge);
      --getSuccessorHead();
    } else
      getSuccessorHead() = edge;
  }

  void addPredecessor(BasicBlock *bb) {
    pred.emplace_back(nullptr, this);
    pred.back() = bb;
  }

  void removePredecessor(BasicBlock *bb) {
    pred.erase(std::remove_if(pred.begin(), pred.end(), // NOLINT
                              [=](auto &elem) { return elem.from == bb; }));
  }

  struct SuccessorView {
    const ListIterator<BasicBlockEdge> &succ_head;
    ListIterator<BasicBlockEdge> begin() { return succ_head; }
    ListIterator<BasicBlockEdge> end() {
      return succ_head.null_end(); // NOLINT
    }
  };

  SuccessorView getSuccessor() { return SuccessorView{getSuccessorHead()}; }

  [[nodiscard]] Instruction *getTerminator() const {
    if (insn.empty() || !insn.back().isControlInstruction())
      return nullptr;
    return &insn.back();
  }

  [[nodiscard]] bool isNormalBasicBlock() const {
    return insn.back().op == OP_Jump || insn.back().op == OP_Branch;
  }

  [[nodiscard]] bool isTerminatorBasicBlock() const {
    return insn.back().op == OP_Return;
  }

  [[nodiscard]] bool isEntryBlock() const;

  void linkByBranch(Value *cond, BasicBlock *true_bb, BasicBlock *false_bb) {
    Instruction::create(OP_Branch, PredefinedType::Void,
                        {cond, true_bb, false_bb}, this);
  }
  void linkByJump(BasicBlock *next_bb) {
    Instruction::create(OP_Jump, PredefinedType::Void, {next_bb}, this);
  }

  bool &refVisited() { return visited; }

  unsigned &refOrder() { return order; }
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

struct ConstantFloat : public Value {
  THIS(SV_ConstantFloat);
  float value;
  ConstantFloat() : Value{this_type} {}
  static ConstantFloat *create(float value) {
    auto *p = new ConstantFloat();
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
    bb->refParent() = this;
  }
};

struct Undef : public Value {
  THIS(SV_Undef);
  Undef() : Value{this_type} {}
  static Undef *create() { return new Undef(); }
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

} // namespace SyOC