#pragma once

#include "IR/IR.hpp"
#include "Tree/Tree.hpp"

class Tree2SSA {
  NodePtr root;

public:
  Tree2SSA(){};
  IRHost *operator()(NodePtr tree) {
    root = tree;
    return nullptr;
  }
};