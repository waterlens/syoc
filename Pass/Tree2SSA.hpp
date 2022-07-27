#pragma once

#include "IR/IR.hpp"
#include "Util/Scope.hpp"
#include "Util/TrivialValueVector.hpp"

#include <algorithm>
#include <cassert>
#include <cstddef>
#include <climits>
#include <functional>
#include <limits>
#include <numeric>
#include <stdexcept>
#include <string_view>
#include <tuple>
#include <utility>
#include <vector>

namespace SyOC {
class Tree2SSA final {
  using Dimension = TrivialValueVector<unsigned>;
  using TypeDimension = std::pair<Type, Dimension>;
  using TypeDimensionValue = std::pair<TypeDimension, Value *>;
  NodePtr root;
  IRHost *host;
  BasicBlock *global_initializer_block, *current_return_bb, *current_alloca_bb,
    *current_entry_bb, *current_bb;
  Function *current_function;
  Instruction *current_retval;
  Scope<TypeDimensionValue> scopes;
  std::vector<BasicBlock *> bb_break, bb_continue;

  void setupGlobalInitializerFunction();
  static TypeDimension convertType(const TreeType &ty) {
    TypeDimension ty_dim;
    if (ty.spec == TS_Int)
      ty_dim.first = PredefinedType::Int32;
    else if (ty.spec == TS_Void)
      ty_dim.first = PredefinedType::Void;
    for (auto *dim : ty.dim) {
      auto v = dim->as<TreeIntegerLiteral *>()->value;
      if (v == std::numeric_limits<decltype(v)>::max())
        v = std::numeric_limits<unsigned>::max();
      ty_dim.second.push_back(v);
    }
    return ty_dim;
  }
  static size_t calculateArrayTotalLength(const TypeDimension &td) {
    return std::accumulate(td.second.begin(), td.second.end(),
                           calculateArrayBaseUnitSize(td), std::multiplies<>());
  }
  static size_t calculateArrayBaseUnitSize(const TypeDimension &td) {
    return static_cast<size_t>(td.first.width) / CHAR_BIT *
           (td.first.primitive_type == Type::PrimitiveType::Void ? 0 : 1);
  }
  TypeDimensionValue findInScope(std::string_view name);
  TypeDimensionValue generateLValue(ExprPtr expr);
  TypeDimensionValue generateShortCircuit(TreeBinaryExpr *binary);
  TypeDimensionValue generateRValue(ExprPtr expr);
  TypeDimensionValue generateArgumentValue(ExprPtr expr);
  void arrayZeroInitializer(const TypeDimensionValue &th);
  void generateStore(ExprPtr expr, Value *target);
  TypeDimensionValue generateLoad(const TypeDimension &td, Value *target);
  void generateListInitializer(TreeInitListExpr *init,
                               const TypeDimensionValue &th);
  void generateInitializer(ExprPtr init, const TypeDimensionValue &th);
  void generateGlobalVariable(TreeGlobalDeclaration *decl);
  void globalGeneration();
  void generateStatement(NodePtr stmt);
  void functionGeneration(TreeFunctionDeclaration *decl);

public:
  Tree2SSA() = default;
  [[nodiscard]] static std::string_view getName() { return "Tree to SSA"; }
  void operator()(const NodePtr &tree, IRHost *&out) {
    host = new IRHost();
    root = tree;
    globalGeneration();
    out = host;
  }
};
} // namespace SyOC