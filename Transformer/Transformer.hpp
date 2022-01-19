#pragma once
#include <chrono>
#include <fmt/format.h>
#include <fmt/chrono.h>
#include <functional>
#include <initializer_list>
#include <stdexcept>
#include <string_view>
#include <vector>


#include "Tree/Tree.hpp"

class Transformer {
public:
  using TreeTransformationFunction = std::function<NodePtr(NodePtr)>;

private:
  NodePtr tree;
  std::vector<std::tuple<std::string_view, TreeTransformationFunction>>
    transformations;

public:
  Transformer(NodePtr tree) : tree(tree) {}

  void registerTreeTransformation(
    std::initializer_list<
      std::tuple<std::string_view, TreeTransformationFunction>>
      list) {
    for (auto &&[name, func] : list) transformations.emplace_back(name, func);
  }

  void transform() {
    if (!tree)
      throw std::runtime_error("tree is null");
    for (auto &&[name, func] : transformations) {
      auto t1 = std::chrono::steady_clock::now();
      auto new_tree = func(tree);
      if (!new_tree)
        throw std::runtime_error(
          fmt::format("tree pass returned empty tree", name));
      tree = new_tree;
      auto t2 = std::chrono::steady_clock::now();
      fmt::print("{}: {:%Q%q}\n", name,
                 std::chrono::duration<double>(t2 - t1));
    }
  }
};