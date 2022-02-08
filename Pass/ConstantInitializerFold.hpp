#pragma once

#include "PassBase.hpp"

#include <cassert>
#include <cstddef>
#include <cstdint>
#include <stdexcept>
#include <string_view>
#include <unordered_set>

class ConstantInitializerFold final
  : public TreeTransformation<ConstantInitializerFold> {
private:
  NodePtr root;

  static bool isConstantExpression(ExprPtr expr) {
    return expr->is<IntegerLiteral *>() || expr->is<RefExpr *>() ||
           expr->is<BinaryExpr *>() || expr->is<UnaryExpr *>();
  }

  void constantInitialization(NodePtr node) {
    assert(isVariableDeclaration(node));
    auto *decl = node->as_unchecked<VariableDeclaration *>();
    if (decl->initializer == nullptr && decl->type.qual == TQ_Const)
      throw std::runtime_error(
        fmt::format("const variable {} must be initialized", decl->name));
    if (decl->initializer != nullptr && decl->type.qual == TQ_Const) {
      if (isConstantExpression(decl->initializer))
        decl->initializer = constantEvaluation(decl->initializer);
      else if (decl->initializer->is<InitListExpr *>())
        decl->initializer = constEvaluationInitList(
          decl->initializer->as_unchecked<InitListExpr *>());
    }
  }

  InitListExpr *constEvaluationInitList(InitListExpr *init_list) {
    for (auto &value : init_list->values) {
      if (isConstantExpression(value))
        value = constantEvaluation(value);
      else if (value->is<InitListExpr *>())
        value = constEvaluationInitList(value->as_unchecked<InitListExpr *>());
    }
    return init_list;
  }

  IntegerLiteral *constantEvaluation(ExprPtr node) {
    assert(isConstantExpression(node));
    if (auto *lit = node->as<IntegerLiteral *>())
      return lit;
    if (auto *ref = node->as<RefExpr *>()) {
      auto *ref_node = ref->decl;
      if (ref_node == nullptr)
        throw std::runtime_error(
          fmt::format("variable {} not found", ref->name));
      if (ref_node->is<FunctionDeclaration *>())
        throw std::runtime_error(fmt::format(
          "the parameter of function is not a constant", ref->name));
      if (!ref_node->is<GlobalDeclaration *>() &&
          !ref_node->is<LocalDeclaration *>())
        throw std::runtime_error(
          fmt::format("{} is not a variable", ref->name));
      auto *decl = ref_node->as_unchecked<VariableDeclaration *>();
      if (!decl->initializer->is<IntegerLiteral *>())
        throw std::runtime_error(fmt::format(
          "variable {} must be initialized as a literal", decl->name));
      return decl->initializer->as_unchecked<IntegerLiteral *>();
    }
    if (auto *unary = node->as<UnaryExpr *>()) {
      auto *result = new IntegerLiteral{0};
      if (unary->op == OP_Neg)
        result->value = -constantEvaluation(unary->operand)->value;
      else if (unary->op == OP_Lnot)
        result->value = static_cast<uint64_t>(
          constantEvaluation(unary->operand)->value == 0U);
      else
        throw std::runtime_error("not a valid unary operator");
      return result;
    }
    if (auto *bin = node->as<BinaryExpr *>()) {
      auto *lhs = constantEvaluation(bin->lhs);
      auto *rhs = constantEvaluation(bin->rhs);
      return new IntegerLiteral{[=]() -> int64_t {
        switch (bin->op) {
        case OP_Add:
          return lhs->value + rhs->value;
        case OP_Sub:
          return lhs->value - rhs->value;
        case OP_Mod:
          if (rhs->value == 0)
            throw std::runtime_error("modulo by zero when constant evaluating");
          return lhs->value % rhs->value;
        case OP_Mul:
          return lhs->value * rhs->value;
        case OP_Div:
          if (rhs->value == 0)
            throw std::runtime_error("divide by zero when constant evaluating");
          return lhs->value / rhs->value;
        case OP_Lt:
          return static_cast<int64_t>(lhs->value < rhs->value);
        case OP_Gt:
          return static_cast<int64_t>(lhs->value > rhs->value);
        case OP_Le:
          return static_cast<int64_t>(lhs->value <= rhs->value);
        case OP_Ge:
          return static_cast<int64_t>(lhs->value >= rhs->value);
        case OP_Eq:
          return static_cast<int64_t>(lhs->value == rhs->value);
        case OP_Ne:
          return static_cast<int64_t>(lhs->value != rhs->value);
        case OP_Land:
          return static_cast<int64_t>((lhs->value != 0) && (rhs->value != 0));
        case OP_Lor:
          return static_cast<int64_t>((lhs->value != 0) || (rhs->value != 0));
        default:
          assert(0);
          return 0;
        }
      }()};
    }

    assert(0);
    return nullptr;
  }

  void arrayDimensionConstantEvaluation(std::vector<ExprPtr> &dim) {
    for (auto &iter : dim) {
      if (!isConstantExpression(iter))
        throw std::runtime_error("not a valid array dimmension");
      iter = constantEvaluation(iter);
    }
  }

  void compoundIteration(CompoundStmt *node) {
    for (auto &stmt : node->stmts)
      if (isVariableDeclaration(stmt)) {
        constantInitialization(stmt);
        arrayDimensionConstantEvaluation(
          stmt->as_unchecked<VariableDeclaration *>()->type.dim);
      } else if (stmt->is<CompoundStmt *>())
        compoundIteration(stmt->as_unchecked<CompoundStmt *>());
  }

  void functionIteration(FunctionDeclaration *func) {
    if (func->body == nullptr)
      return;
    for (auto &&[name, type] : func->parameters)
      arrayDimensionConstantEvaluation(type.dim);
    auto *body = func->body->as<CompoundStmt *>();
    compoundIteration(body);
  }

  void globalIteration() {
    auto *module = root->as<Module *>();
    for (auto *decl : module->decls) {
      if (isVariableDeclaration(decl)) {
        constantInitialization(decl);
        arrayDimensionConstantEvaluation(
          decl->as_unchecked<VariableDeclaration *>()->type.dim);
      } else if (isFunctionDeclaration(decl)) {
        auto *f = decl->as_unchecked<FunctionDeclaration *>();
        functionIteration(f);
      } else {
        throw std::runtime_error("unknown node in the top level of module");
      }
    }
  }

public:
  ConstantInitializerFold() = default;
  [[nodiscard]] static std::string_view getName()  {
    return "Constant Initializer Fold";
  }
  void operator()(NodePtr &tree) {
    root = tree;
    globalIteration();
    tree = root;
  }
};