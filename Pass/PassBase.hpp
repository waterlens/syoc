#pragma once

#include "IR/IR.hpp"
#include "Tree/Tree.hpp"
#include <string_view>

template <typename T> class TransformationBase {
public:
  [[nodiscard]] std::string_view getName() const {
    return static_cast<const T *>(this)->getName();
  }
};

template <typename T> class TreeTransformation : public TransformationBase<T> {
public:
  void operator()(NodePtr &root) { static_cast<T *>(this)->operator()(root); }
};

template <typename T>
class Tree2SSATransformation : public TransformationBase<T> {
public:
  void operator()(const NodePtr &root, IRHost *&out) {
    static_cast<T *>(this)->operator()(root, out);
  }
};

template <typename T> class SSATransformation : public TransformationBase<T> {
public:
  void operator()(IRHost &host) {
    static_cast<T *>(this)->operator()(host);
  }
};

template <typename T> class SSAAnalysis : public SSATransformation<T> {};