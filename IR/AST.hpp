#pragma once
#include "IR/Variable.hpp"
#include <list>
#include <string>

enum NodeType {
  CALL,
  MUL,
  DIV,
  MOD,
  ADD,
  SUB,
  LT,
  GT,
  LE,
  GE,
  EQ,
  NE,
  LAND,
  LOR,
};

struct Node {
  virtual std::string to_string() const;
  virtual bool is_if() const { return false; }
  virtual bool is_while() const { return false; }
  virtual bool is_operation() const { return false; }
  virtual bool is_block() const { return false; }
  virtual bool is_call() const { return false; }
  virtual bool is_leaf() const { return false; }
};

struct IfNode : public Node {
  Node *cond, *then, *els;
  bool is_if() const override { return true; }
};

struct WhileNode : public Node {
  Node *cond, *body;
  bool is_while() const override { return true; }
};

struct OperationNode : public Node {
  NodeType type;
  Node *lhs, *rhs;
  bool is_operation() const override { return true; }
};

struct BlockNode : public Node {
  std::list<Node *> blk;
  bool is_block() const override { return true; }
};

struct CallNode : public Node {
  Node *func, *args;
  bool is_call() const override { return true; }
};

struct LeafNode : public Node {
  Value *value;
  bool is_leaf() const override { return true; }
};