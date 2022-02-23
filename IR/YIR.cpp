#include "YIR.hpp"

namespace YIR {

UseEdge::UseEdge(Value *from, Value *to) {
  this->from = from;
  this->to = to;
  if (from != nullptr)
    from->addEdge(this);
}

UseEdge::~UseEdge() {
  if (from != nullptr)
    from->removeEdge(this);
}

BasicBlockEdge::BasicBlockEdge(BasicBlock *from, BasicBlock *to) {
  this->from = from;
  this->to = to;
  if (from != nullptr)
    from->addSuccessor(this);
}

BasicBlockEdge::~BasicBlockEdge() {
  if (from != nullptr)
    from->removeSuccessor(this);
}

std::function<void(BasicBlockEdge *, BasicBlock *, BasicBlock *)>
  BasicBlock::addPredecessorAction =
    [](BasicBlockEdge *edge, BasicBlock *from, BasicBlock *) {
      from->addSuccessor(edge);
    };

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
    p->input.emplace_back(nullptr, nullptr);
    p->getLastInput().associate(input);
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
  for (; getSuccessor().base() != nullptr;) {
    getSuccessor()->to->removePredecessor(this);
    // then to node will destruct the edge
    // which will call removeSuccessor of this
  }
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

} // namespace YIR