#pragma once

#include "Tree/Tree.hpp"
#include <cassert>
#include <cstddef>
#include <cstdint>
#include <stdexcept>
#include <string_view>
#include <unordered_set>
#include <variant>

namespace SyOC {
class ConstantInitializerFold final {
private:
  NodePtr root;

  using LiteralPack = std::tuple<unsigned, float, int64_t>;

  static bool isConstantExpression(ExprPtr expr) {
    return expr->is<TreeIntegerLiteral *>() || expr->is<TreeFloatLiteral *>() ||
           expr->is<TreeRefExpr *>() || expr->is<TreeBinaryExpr *>() ||
           expr->is<TreeUnaryExpr *>();
  }

  void constantInitialization(NodePtr node) {
    assert(isVariableDeclaration(node));
    auto *decl = node->as_unchecked<TreeVariableDeclaration *>();
    if (decl->initializer == nullptr && decl->type.qual == TQ_Const)
      throw std::runtime_error(
        fmt::format("const variable {} must be initialized", decl->name));
    if (decl->initializer != nullptr && decl->type.qual == TQ_Const) {
      if (isConstantExpression(decl->initializer))
        decl->initializer = literalPackToTreeExpr(
          constantEvaluation(decl->initializer), decl->type.spec);
      else if (decl->initializer->is<TreeInitListExpr *>())
        decl->initializer = constEvaluationInitList(
          decl->initializer->as_unchecked<TreeInitListExpr *>(),
          decl->type.spec);
    }
  }

  TreeInitListExpr *constEvaluationInitList(TreeInitListExpr *init_list,
                                            TypeSpecifier spec) {
    for (auto &value : init_list->values) {
      if (isConstantExpression(value))
        value = literalPackToTreeExpr(constantEvaluation(value), spec);
      else if (value->is<TreeInitListExpr *>())
        value = constEvaluationInitList(
          value->as_unchecked<TreeInitListExpr *>(), spec);
    }
    return init_list;
  }

  static ExprPtr literalPackToTreeExpr(LiteralPack pack, TypeSpecifier spec) {
    auto [idx, f, i] = pack;
    switch (spec) {
    case TS_Int:
      return new TreeIntegerLiteral{static_cast<int64_t>(idx == 1 ? i : f)};
    case TS_Float:
      return new TreeFloatLiteral{idx == 1 ? i : f};
    case TS_None:
    case TS_Void:
    default:
      throw std::runtime_error("wrong type of initializer");
    }
  }

  static LiteralPack foldFloatBinExpr(OpType op, float lhs, float rhs) {
    switch (op) {
    case OP_Add:
      return makeFloatLiteralPack(lhs + rhs);
    case OP_Sub:
      return makeFloatLiteralPack(lhs - rhs);
    case OP_Mod:
      throw std::runtime_error("float modulo is not allowed");
    case OP_Mul:
      return makeFloatLiteralPack(lhs * rhs);
    case OP_Div:
      return makeFloatLiteralPack(lhs / rhs);
    case OP_Lt:
      return makeIntegerLiteralPack(static_cast<int64_t>(lhs < rhs));
    case OP_Gt:
      return makeIntegerLiteralPack(static_cast<int64_t>(lhs > rhs));
    case OP_Le:
      return makeIntegerLiteralPack(static_cast<int64_t>(lhs <= rhs));
    case OP_Ge:
      return makeIntegerLiteralPack(static_cast<int64_t>(lhs >= rhs));
    case OP_Eq:
      return makeIntegerLiteralPack(static_cast<int64_t>(lhs == rhs));
    case OP_Ne:
      return makeIntegerLiteralPack(static_cast<int64_t>(lhs != rhs));
    case OP_Land:
      return makeIntegerLiteralPack(static_cast<int64_t>(lhs != 0 && rhs != 0));
    case OP_Lor:
      return makeIntegerLiteralPack(static_cast<int64_t>(lhs != 0 || rhs != 0));
    default:
      throw std::runtime_error("not allowed operation");
    }
  }

  static LiteralPack foldIntegerBinExpr(OpType op, int64_t lhs, int64_t rhs) {
    switch (op) {
    case OP_Add:
      return makeIntegerLiteralPack(lhs + rhs);
    case OP_Sub:
      return makeIntegerLiteralPack(lhs - rhs);
    case OP_Mod:
      if (rhs == 0)
        throw std::runtime_error("modulo by zero when constant evaluating");
      return makeIntegerLiteralPack(lhs % rhs);
    case OP_Mul:
      return makeIntegerLiteralPack(lhs * rhs);
    case OP_Div:
      if (rhs == 0)
        throw std::runtime_error("divide by zero when constant evaluating");
      return makeIntegerLiteralPack(lhs / rhs);
    case OP_Lt:
      return makeIntegerLiteralPack(static_cast<int64_t>(lhs < rhs));
    case OP_Gt:
      return makeIntegerLiteralPack(static_cast<int64_t>(lhs > rhs));
    case OP_Le:
      return makeIntegerLiteralPack(static_cast<int64_t>(lhs <= rhs));
    case OP_Ge:
      return makeIntegerLiteralPack(static_cast<int64_t>(lhs >= rhs));
    case OP_Eq:
      return makeIntegerLiteralPack(static_cast<int64_t>(lhs == rhs));
    case OP_Ne:
      return makeIntegerLiteralPack(static_cast<int64_t>(lhs != rhs));
    case OP_Land:
      return makeIntegerLiteralPack(static_cast<int64_t>(lhs != 0 && rhs != 0));
    case OP_Lor:
      return makeIntegerLiteralPack(static_cast<int64_t>(lhs != 0 || rhs != 0));
    default:
      throw std::runtime_error("not allowed operation");
    }
  }

  static LiteralPack makeFloatLiteralPack(float f) { return {0, f, 0}; }
  static LiteralPack makeIntegerLiteralPack(int64_t i) { return {1, 0, i}; }

  LiteralPack constantEvaluation(ExprPtr node) {
    assert(isConstantExpression(node));
    if (auto *lit = node->as<TreeIntegerLiteral *>())
      return makeIntegerLiteralPack(lit->value);

    if (auto *lit = node->as<TreeFloatLiteral *>())
      return makeFloatLiteralPack(lit->value);

    if (auto *ref = node->as<TreeRefExpr *>()) {
      auto *ref_node = ref->decl;
      if (ref_node == nullptr)
        throw std::runtime_error(
          fmt::format("variable {} not found", ref->name));
      if (ref_node->is<TreeFunctionDeclaration *>())
        throw std::runtime_error(fmt::format(
          "the parameter of function is not a constant", ref->name));
      if (!ref_node->is<TreeGlobalDeclaration *>() &&
          !ref_node->is<TreeLocalDeclaration *>())
        throw std::runtime_error(
          fmt::format("{} is not a variable", ref->name));
      auto *decl = ref_node->as_unchecked<TreeVariableDeclaration *>();
      if (auto *lit = decl->initializer->as<TreeIntegerLiteral *>())
        return makeIntegerLiteralPack(lit->value);

      if (auto *lit = decl->initializer->as<TreeFloatLiteral *>())
        return makeFloatLiteralPack(lit->value);

      throw std::runtime_error(fmt::format(
        "variable {} must be initialized as a literal", decl->name));
    }

    if (auto *unary = node->as<TreeUnaryExpr *>()) {
      if (unary->op == OP_Neg) {
        auto [idx, f, i] = constantEvaluation(unary->operand);
        return {idx, -f, -i};
      }
      if (unary->op == OP_Lnot) {
        auto [idx, f, i] = constantEvaluation(unary->operand);
        return makeIntegerLiteralPack(
          static_cast<int64_t>(idx == 1 ? i == 0 : f == 0));
      }
      throw std::runtime_error("not a valid unary operator");
    }

    if (auto *bin = node->as<TreeBinaryExpr *>()) {
      auto [lty, lf, li] = constantEvaluation(bin->lhs);
      auto [rty, rf, ri] = constantEvaluation(bin->rhs);

      if (lty == 0 || rty == 0) {
        // one operator is float
        return foldFloatBinExpr(bin->op, lty == 1 ? li : lf,
                                rty == 1 ? ri : rf);
      }
      return foldIntegerBinExpr(bin->op, lty == 1 ? li : lf,
                                rty == 1 ? ri : rf);
    }

    throw std::runtime_error("not a constant expression");
  }

  void arrayDimensionConstantEvaluation(std::vector<ExprPtr> &dim) {
    for (auto &iter : dim) {
      if (!isConstantExpression(iter))
        throw std::runtime_error("not a valid array dimmension");
      iter = literalPackToTreeExpr(constantEvaluation(iter), TS_Int);
    }
  }

  void compoundIteration(TreeCompoundStmt *node) {
    for (auto &stmt : node->stmts)
      if (isVariableDeclaration(stmt)) {
        constantInitialization(stmt);
        arrayDimensionConstantEvaluation(
          stmt->as_unchecked<TreeVariableDeclaration *>()->type.dim);
      } else if (stmt->is<TreeCompoundStmt *>())
        compoundIteration(stmt->as_unchecked<TreeCompoundStmt *>());
  }

  void functionIteration(TreeFunctionDeclaration *func) {
    if (func->body == nullptr)
      return;
    for (auto &&[name, type] : func->parameters)
      arrayDimensionConstantEvaluation(type.dim);
    auto *body = func->body->as<TreeCompoundStmt *>();
    compoundIteration(body);
  }

  void globalIteration() {
    auto *module = root->as<TreeModule *>();
    for (auto *decl : module->decls) {
      if (isVariableDeclaration(decl)) {
        constantInitialization(decl);
        arrayDimensionConstantEvaluation(
          decl->as_unchecked<TreeVariableDeclaration *>()->type.dim);
      } else if (isFunctionDeclaration(decl)) {
        auto *f = decl->as_unchecked<TreeFunctionDeclaration *>();
        functionIteration(f);
      } else {
        throw std::runtime_error("unknown node in the top level of module");
      }
    }
  }

public:
  ConstantInitializerFold() = default;
  [[nodiscard]] static std::string_view getName() {
    return "Constant Initializer Fold";
  }
  void operator()(NodePtr &tree) {
    root = tree;
    globalIteration();
    tree = root;
  }
};
} // namespace SyOC