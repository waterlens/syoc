#pragma once
#include <chrono>
#include <fmt/chrono.h>
#include <fmt/format.h>
#include <functional>
#include <initializer_list>
#include <stdexcept>
#include <string_view>
#include <vector>

#include "IR/IR.hpp"
#include "Tree/Tree.hpp"

class Transformer {
public:
  using TreeTransformationFunction = std::function<NodePtr(NodePtr)>;
  using SSATransformationFunction = std::function<void(IRHost &)>;
  using Tree2SSATransformationFunction = std::function<IRHost *(NodePtr)>;

private:
  NodePtr tree;
  IRHost *host;
  std::vector<std::tuple<std::string_view, TreeTransformationFunction>>
    tree_transformation;
  std::vector<std::tuple<std::string_view, SSATransformationFunction>>
    ssa_transformation;
  Tree2SSATransformationFunction tree2ssa_transformation;

public:
  Transformer(NodePtr tree) : tree(tree) {}

  void registerTreeTransformation(
    std::initializer_list<
      std::tuple<std::string_view, TreeTransformationFunction>>
      list) {
    for (auto &&[name, func] : list)
      tree_transformation.emplace_back(name, func);
  }

  void registerSSATransformation(
    std::initializer_list<
      std::tuple<std::string_view, SSATransformationFunction>>
      list) {
    for (auto &&[name, func] : list)
      ssa_transformation.emplace_back(name, func);
  }

  void registerTree2SSATransformation(Tree2SSATransformationFunction func) {
    tree2ssa_transformation = func;
  }

  void transform() {
    if (!tree)
      throw std::runtime_error("tree is null");
    for (auto &&[name, func] : tree_transformation) {
      auto t1 = std::chrono::steady_clock::now();
      tree = func(tree);
      auto t2 = std::chrono::steady_clock::now();
      fmt::print("{}: {:%Q%q}\n", name, std::chrono::duration<double>(t2 - t1));
    }

    auto t1 = std::chrono::steady_clock::now();
    host = tree2ssa_transformation(tree);
    auto t2 = std::chrono::steady_clock::now();
    fmt::print("{}: {:%Q%q}\n", "Tree to SSA",
               std::chrono::duration<double>(t2 - t1));
    for (auto &&[name, func] : ssa_transformation) {
      auto t1 = std::chrono::steady_clock::now();
      func(*host);
      auto t2 = std::chrono::steady_clock::now();
      fmt::print("{}: {:%Q%q}\n", name, std::chrono::duration<double>(t2 - t1));
    }
  }
};