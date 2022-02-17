#pragma once
#include <chrono>
#include <fmt/chrono.h>
#include <fmt/format.h>
#include <functional>
#include <initializer_list>
#include <memory>
#include <stdexcept>
#include <string_view>
#include <type_traits>
#include <utility>
#include <vector>

#include "IR/IR.hpp"
#include "IR/YIR.hpp"
#include "Pass/PassBase.hpp"
#include "Pass/Tree2SSA.hpp"
#include "Tree/Tree.hpp"

class Transformer {
private:
  NodePtr tree;
  IRHost *host;

  static void run(std::function<void()> &&func, std::string_view name) {
    auto t1 = std::chrono::steady_clock::now();
    func();
    auto t2 = std::chrono::steady_clock::now();
    fmt::print("{}: {:%Q%q}\n", name, std::chrono::duration<double>(t2 - t1));
  }

public:
  Transformer(NodePtr tree) : tree(tree) {}
  template <typename T> void doTreeTransformation() {
    T f{};
    run([&]() { f(tree); }, f.getName());
  }

  template <typename T> void doSSATransformation() {
    T f{};
    run([&]() { f(*host); }, f.getName());
  }

  template <typename T> void doTree2SSATransformation() {
    T f{};
    run([&]() { f(tree, host); }, f.getName());
  }

  template <typename T1, typename T2, typename... Tail>
  void doTreeTransformation() {
    doTreeTransformation<T1>();
    doTreeTransformation<T2, Tail...>();
  }

  template <typename T1, typename T2, typename... Tail>
  void doSSATransformation() {
    doSSATransformation<T1>();
    doSSATransformation<T2, Tail...>();
  }

  template <typename T1, typename T2, typename... Tail>
  void doTree2SSATransformation() {
    doTree2SSATransformation<T1>();
    doTree2SSATransformation<T2, Tail...>();
  }
};

namespace YIR {
class Transformer {
private:
  NodePtr tree;
  IRHost *host;

  static void run(std::function<void()> &&func, std::string_view name) {
    auto t1 = std::chrono::steady_clock::now();
    func();
    auto t2 = std::chrono::steady_clock::now();
    fmt::print("{}: {:%Q%q}\n", name, std::chrono::duration<double>(t2 - t1));
  }

public:
  Transformer(NodePtr tree) : tree(tree) {}
  template <typename T> void doTreeTransformation() {
    T f{};
    run([&]() { f(tree); }, f.getName());
  }

  template <typename T> void doSSATransformation() {
    T f{};
    run([&]() { f(*host); }, f.getName());
  }

  template <typename T> void doTree2SSATransformation() {
    T f{};
    run([&]() { f(tree, host); }, f.getName());
  }

  template <typename T1, typename T2, typename... Tail>
  void doTreeTransformation() {
    doTreeTransformation<T1>();
    doTreeTransformation<T2, Tail...>();
  }

  template <typename T1, typename T2, typename... Tail>
  void doSSATransformation() {
    doSSATransformation<T1>();
    doSSATransformation<T2, Tail...>();
  }

  template <typename T1, typename T2, typename... Tail>
  void doTree2SSATransformation() {
    doTree2SSATransformation<T1>();
    doTree2SSATransformation<T2, Tail...>();
  }
};
} // namespace YIR