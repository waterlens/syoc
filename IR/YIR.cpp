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
    p->getLastInput() = input;
  }
  if (bb != nullptr) {
    bb->insn.push_back(p);
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