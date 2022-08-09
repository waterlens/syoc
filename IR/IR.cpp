#include "IR.hpp"
#include <cassert>

namespace SyOC {

UseEdge::UseEdge(Value *from, Value *to) {
  this->from = from;
  this->to = to;
  if (from != nullptr)
    from->addEdge(this);
}

UseEdge::UseEdge(UseEdge &&edge) noexcept {
  from = edge.from;
  to = edge.to;
  assert(from && to);
  edge.from = nullptr;
  edge.to = nullptr;
  from->removeEdge(&edge);
  from->addEdge(this);
}

UseEdge::~UseEdge() {
  if (from != nullptr)
    from->removeEdge(this);
}

void UseEdge::associate(Value *from) {
  if (this->from != nullptr)
    this->from->removeEdge(this);
  this->from = from;
  if (from != nullptr)
    from->addEdge(this);
}

UseEdge &UseEdge::operator=(Value *from) {
  associate(from);
  return *this;
}

UseEdge &UseEdge::operator=(UseEdge &&edge) noexcept {
  if (this != &edge) {
    if (from != nullptr)
      from->removeEdge(this);
    from = edge.from;
    to = edge.to;
    edge.from = nullptr;
    edge.to = nullptr;
    from->removeEdge(&edge);
    from->addEdge(this);
  }
  return *this;
}

BasicBlockEdge::BasicBlockEdge(BasicBlock *from, BasicBlock *to) {
  this->from = from;
  this->to = to;
  if (from != nullptr)
    from->addSuccessor(this);
}

BasicBlockEdge::BasicBlockEdge(BasicBlockEdge &&edge) noexcept {
  from = edge.from;
  to = edge.to;
  edge.from = nullptr;
  edge.to = nullptr;
  from->removeSuccessor(&edge);
  from->addSuccessor(this);
}

BasicBlockEdge::~BasicBlockEdge() {
  if (from != nullptr)
    from->removeSuccessor(this);
}

void BasicBlockEdge::associate(BasicBlock *from) {
  if (this->from != nullptr)
    this->from->removeSuccessor(this);
  this->from = from;
  if (from != nullptr)
    from->addSuccessor(this);
}

BasicBlockEdge &BasicBlockEdge::operator=(BasicBlock *from) {
  associate(from);
  return *this;
}

BasicBlockEdge &BasicBlockEdge::operator=(BasicBlockEdge &&edge) noexcept {
  if (this != &edge) {
    if (from != nullptr)
      from->removeSuccessor(this);
    from = edge.from;
    to = edge.to;
    edge.from = nullptr;
    edge.to = nullptr;
    from->removeSuccessor(&edge);
    from->addSuccessor(this);
  }
  return *this;
}

Instruction *Instruction::create(OpType op, Type type,
                                 std::initializer_list<Value *> inputs,
                                 BasicBlock *bb) {
  auto *p = new Instruction();
  p->op = op;
  p->type = type;
  p->parent = bb;
  p->m_next = nullptr;
  p->m_prev = nullptr;
  for (auto *input : inputs) {
    p->input.emplace_back(nullptr, p);
    p->input.back() = input;
  }
  if (bb != nullptr) {
    bb->getInstruction().push_back(p);
  }
  return p;
}

Instruction *Instruction::create(OpType op, Type type,
                                 const std::vector<Value *> &Vec,
                                 BasicBlock *bb) {
  auto *p = new Instruction();
  p->op = op;
  p->type = type;
  p->parent = bb;
  p->m_next = nullptr;
  p->m_prev = nullptr;
  for (auto *input : Vec) {
    p->input.emplace_back(nullptr, p);
    p->input.back() = input;
  }
  if (bb != nullptr) {
    bb->getInstruction().push_back(p);
  }
  return p;
}

BasicBlock *BasicBlock::create(Function *f) {
  auto *p = new BasicBlock();
  p->parent = f;
  p->m_next = nullptr;
  p->m_prev = nullptr;
  if (f != nullptr) {
    f->block.push_back(p);
  }
  return p;
}

bool BasicBlock::isEntryBlock() const {
  assert(parent != nullptr);
  auto *p = parent->as<Function *>();
  if (p->refExternal() || p->block.empty())
    return false;
  return this == &p->block.front();
}

BasicBlock::~BasicBlock() {
  for (; getSuccessorHead().base() != nullptr;) {
    getSuccessorHead()->to->removePredecessor(this);
    // then to node will destruct the edge
    // which will call removeSuccessor of this
  }
  /// @attention duplicated remove from list.
  remove_from_list();
}

Argument *Argument::create(Type type, std::string_view name,
                           Function *f = nullptr) {
  auto *p = new Argument();
  p->type = type;
  p->name = name;
  p->parent = f;
  if (f != nullptr)
    f->arg.push_back(p);
  return p;
}

Function *Function::create(Type type, std::string_view name, Module *m) {
  auto *p = new Function();
  p->return_type = type;
  p->name = name;
  p->parent = m;
  if (m != nullptr)
    m->func.push_back(p);
  return p;
}

GlobalVariable *GlobalVariable::create(Type type, std::string_view name,
                                       unsigned capacity, Module *m) {
  auto *p = new GlobalVariable();
  p->type = type;
  p->name = name;
  p->capacity = capacity;
  p->parent = m;
  if (m != nullptr)
    m->global.push_back(p);
  return p;
}

} // namespace SyOC