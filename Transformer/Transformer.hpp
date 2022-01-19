#pragma once
#include <fmt/format.h>
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
      auto new_tree = func(tree);
      if (!new_tree)
        throw std::runtime_error(
          fmt::format("tree pass returned empty tree", name));
      tree = new_tree;
    }
  }
};