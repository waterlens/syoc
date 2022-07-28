#pragma once

#include "Tree/Tree.hpp"
#include <cassert>
#include <stdexcept>

namespace SyOC {
class TypeCheck final {
private:
  NodePtr root;
  TreeFunctionDeclaration *current_function;

  static bool isArrayType(const TreeType &type) { return !type.dim.empty(); }

  static bool isPointerLogicOp(OpType op) {
    return op == OP_Land || op == OP_Lor;
  }

  static bool isPointerComparaisonOp(OpType op) {
    return op == OP_Eq || op == OP_Ne || op == OP_Ge || op == OP_Le ||
           op == OP_Gt || op == OP_Lt;
  }

  static bool isPointerArithmeticOp(OpType op) {
    return op == OP_Add || op == OP_Sub;
  }

  static void checkInitializerCompatible(ExprPtr expr, const TreeType &type) {
    if (isArrayType(type) && !expr->is<TreeInitListExpr *>())
      throw std::runtime_error("initializer list required");
    for (auto *dim : type.dim) {
      if (!dim->is<TreeIntegerLiteral *>())
        throw std::runtime_error(
          "array dimension must be non-negative integer");
    }
  }

  static TreeType combine(OpType op, const TreeType &lhs, const TreeType &rhs) {
    if (!isArrayType(lhs) && !isArrayType(rhs))
      return TreeType{TS_Int, TQ_None, {}};
    throw std::runtime_error("pointer arithmetic not allowed");
    // support pointer arithmetic if required
    {
      if (isArrayType(lhs) && !isArrayType(rhs)) {
        if (isPointerArithmeticOp(op))
          return TreeType{TS_Int, TQ_None, lhs.dim};
        if (isPointerComparaisonOp(op) || isPointerLogicOp(op))
          return TreeType{TS_Int, TQ_None, {}};
        throw std::runtime_error("incompatible types when lhs is an array");
      }
      if (!isArrayType(lhs) && isArrayType(rhs)) {
        if (op == OP_Add)
          return TreeType{TS_Int, TQ_None, rhs.dim};
        if (isPointerComparaisonOp(op) || isPointerLogicOp(op))
          return TreeType{TS_Int, TQ_None, {}};
        throw std::runtime_error("incompatible types when rhs is an array");
      }
      // isArrayType(lhs) && isArrayType(rhs)
      if (isPointerComparaisonOp(op) || isPointerLogicOp(op))
        return TreeType{TS_Int, TQ_None, {}};
      if (op == OP_Sub) {
        if (lhs.dim.size() != rhs.dim.size())
          throw std::runtime_error("incompatible array dimensions");
        return TreeType{TS_Int, TQ_None, lhs.dim};
      }
      throw std::runtime_error(
        "unable to apply such an operator on both array types");
    }
  }

  static void checkArrayDimensionViolation(TreeVariableDeclaration *decl) {
    if (decl->type.qual == TQ_Const && decl->initializer == nullptr)
      throw std::runtime_error(
        fmt::format("const variable {} must be initialized", decl->name));
    if (decl->initializer != nullptr)
      checkInitializerCompatible(decl->initializer, decl->type);
  }

  void checkStatement(NodePtr stmt) {
    if (stmt->is<TreeCompoundStmt *>())
      checkCompoundStmt(stmt);
    if (isVariableDeclaration(stmt))
      checkArrayDimensionViolation(
        stmt->as_unchecked<TreeVariableDeclaration *>());
    if (isExpression(stmt))
      checkExpr(stmt->as_unchecked<ExprPtr>());
    if (stmt->is<TreeIfStmt *>())
      checkIfStmt(stmt);
    if (stmt->is<TreeWhileStmt *>())
      checkWhileStmt(stmt);
    if (stmt->is<TreeReturnStmt *>())
      checkReturnStmt(stmt);
  }

  void checkCompoundStmt(NodePtr stmt) {
    assert(stmt->is<TreeCompoundStmt *>());
    for (auto *child : stmt->as_unchecked<TreeCompoundStmt *>()->stmts)
      checkStatement(child);
  }

  void checkIfStmt(NodePtr stmt) {
    assert(stmt->is<TreeIfStmt *>());
    auto *if_stmt = stmt->as_unchecked<TreeIfStmt *>();
    assert(isExpression(if_stmt->condition));
    auto cond_type = checkExpr(if_stmt->condition->as_unchecked<ExprPtr>());
    checkStatement(if_stmt->then_stmt);
    if (if_stmt->else_stmt != nullptr)
      checkStatement(if_stmt->else_stmt);
  }

  void checkWhileStmt(NodePtr stmt) {
    assert(stmt->is<TreeWhileStmt *>());
    auto *while_stmt = stmt->as_unchecked<TreeWhileStmt *>();
    assert(isExpression(while_stmt->condition));
    auto cond_type = checkExpr(while_stmt->condition->as_unchecked<ExprPtr>());
    checkStatement(while_stmt->body);
  }

  void checkReturnStmt(NodePtr stmt) {
    assert(stmt->is<TreeReturnStmt *>());
    assert(current_function);
    auto *ret = stmt->as_unchecked<TreeReturnStmt *>();
    if (current_function->return_type.spec == TS_Void && ret->value != nullptr)
      throw std::runtime_error("void function cannot return a value");
    if (current_function->return_type.spec == TS_Void && ret->value == nullptr)
      return;
    auto ret_type = checkExpr(ret->value);
    if (ret_type.spec == TS_Void)
      throw std::runtime_error("cannot return a void value");
    if (!isTypeSpecifierCompatible(current_function->return_type.spec,
                                   ret_type.spec) ||
        ret_type.dim.size() != current_function->return_type.dim.size())
      throw std::runtime_error("return type mismatch");
  }

  TreeType checkExpr(ExprPtr expr) {
    switch (expr->node_type) {
    case ND_IntegerLiteral:
      return TreeType{TS_Int, TQ_None, {}};
    case ND_FloatLiteral:
      return TreeType{TS_Float, TQ_None, {}};
    case ND_ArraySubscriptExpr:
      return checkArraySubscriptExpr(expr);
    case ND_UnaryExpr:
      return checkUnaryExpr(expr);
    case ND_BinaryExpr:
      return checkBinaryExpr(expr);
    case ND_CallExpr:
      return checkCallExpr(expr);
    case ND_RefExpr:
      return checkRefExpr(expr);
    case ND_AssignExpr:
      return checkAssignExpr(expr);
    default:
      throw std::runtime_error("not a valid expression");
    }
  }

  TreeType checkArraySubscriptExpr(ExprPtr expr) {
    assert(expr->is<TreeArraySubscriptExpr *>());
    auto *array_subscript = expr->as_unchecked<TreeArraySubscriptExpr *>();
    auto *array = array_subscript->array;
    auto *subscript = array_subscript->subscript;
    auto array_type = checkExpr(array);
    auto subscript_type = checkExpr(subscript);
    if (array_type.spec == TS_Void)
      throw std::runtime_error("cannot subscript void");
    if (array_type.dim.empty())
      throw std::runtime_error("cannot subscript scalar");
    if (subscript_type.spec != TS_Int || !subscript_type.dim.empty())
      throw std::runtime_error("array subscript must be integer");
    std::vector<ExprPtr> new_dim;
    for (size_t i = 1; i < array_type.dim.size(); i++)
      new_dim.push_back(array_type.dim[i]);
    return TreeType{array_type.spec, array_type.qual, new_dim};
  }

  TreeType checkUnaryExpr(ExprPtr expr) {
    assert(expr->is<TreeUnaryExpr *>());
    auto *unary = expr->as_unchecked<TreeUnaryExpr *>();
    auto unary_type = checkExpr(unary->operand);
    if (unary_type.spec == TS_Void)
      throw std::runtime_error("cannot apply unary operator to void");
    return TreeType{TS_Int, TQ_None, {}};
  }

  TreeType checkBinaryExpr(ExprPtr expr) {
    assert(expr->is<TreeBinaryExpr *>());
    auto *binary = expr->as_unchecked<TreeBinaryExpr *>();
    auto lhs_type = checkExpr(binary->lhs);
    auto rhs_type = checkExpr(binary->rhs);
    if (lhs_type.spec == TS_Void || rhs_type.spec == TS_Void)
      throw std::runtime_error("cannot apply binary operator to void");
    return combine(binary->op, lhs_type, rhs_type);
  }

  static bool isTypeSpecifierCompatible(TypeSpecifier src, TypeSpecifier dst) {
    return src == dst || src == TS_Int && dst == TS_Float ||
           src == TS_Float && dst == TS_Int;
  }

  static bool isCompatible(const TreeType &a, const TreeType &b) {
    return a.spec == b.spec && a.dim.size() == b.dim.size() ||
           a.dim.size() + b.dim.size() == 0 &&
             isTypeSpecifierCompatible(a.spec, b.spec);
  }

  TreeType checkCallExpr(ExprPtr expr) {
    assert(expr->is<TreeCallExpr *>());
    auto *call = expr->as_unchecked<TreeCallExpr *>();
    auto *func = call->func;
    if (!func->is<TreeRefExpr *>())
      throw std::runtime_error(
        "cannot call non-function (function pointer is not supported)");
    auto *decl = func->as_unchecked<TreeRefExpr *>()->decl;
    if (!decl->is<TreeFunctionDeclaration *>())
      throw std::runtime_error("cannot call non-function");
    auto *func_decl = decl->as_unchecked<TreeFunctionDeclaration *>();
    auto &args = call->args;
    if (args.size() != func_decl->parameters.size())
      throw std::runtime_error(
        fmt::format("{} requires {} arguments but given {}", func_decl->name,
                    func_decl->parameters.size(), args.size()));
    for (size_t i = 0; i < args.size(); i++) {
      auto arg_type = checkExpr(args[i]);
      if (!isCompatible(arg_type, func_decl->parameters[i].second))
        throw std::runtime_error(
          fmt::format("argument is incompatible", i, func_decl->name));
    }
    return func_decl->return_type;
  }

  static TreeType checkRefExpr(ExprPtr expr) {
    assert(expr->is<TreeRefExpr *>());
    auto *ref = expr->as_unchecked<TreeRefExpr *>();
    auto *decl = ref->decl;
    if (isVariableDeclaration(decl)) {
      return decl->as_unchecked<TreeVariableDeclaration *>()->type;
    }
    if (decl->is<TreeFunctionDeclaration *>()) {
      auto *func = decl->as_unchecked<TreeFunctionDeclaration *>();
      if (ref->name == func->name)
        throw std::runtime_error("cannot take address of function (function "
                                 "pointer is not supported)");
      for (auto &param : func->parameters) {
        if (ref->name == param.first)
          return param.second;
      }
    }
    throw std::runtime_error("not a valid name reference");
  }

  TreeType checkAssignExpr(ExprPtr expr) {
    assert(expr->is<TreeAssignExpr *>());
    auto *assign = expr->as_unchecked<TreeAssignExpr *>();
    auto lhs_type = checkExpr(assign->lhs);
    auto rhs_type = checkExpr(assign->rhs);
    if (lhs_type.spec == TS_Void || rhs_type.spec == TS_Void)
      throw std::runtime_error("cannot assign to void or use void as value");
    if (lhs_type.qual == TQ_Const)
      throw std::runtime_error("cannot assign to const");
    if (isArrayType(rhs_type))
      throw std::runtime_error("cannot assign array address to variable");
    return lhs_type;
  }

  void functionIteration(TreeFunctionDeclaration *func) {
    if (func->body == nullptr)
      return;
    auto *body = func->body->as<TreeCompoundStmt *>();
    current_function = func;
    checkCompoundStmt(body);
    current_function = nullptr;
  }

  void globalIteration() {
    auto *module = root->as<TreeModule *>();
    for (auto *decl : module->decls) {
      if (decl->is<TreeGlobalDeclaration *>()) {
        auto *p = decl->as_unchecked<TreeGlobalDeclaration *>();
        checkArrayDimensionViolation(p);
      } else if (isFunctionDeclaration(decl)) {
        auto *f = decl->as_unchecked<TreeFunctionDeclaration *>();
        functionIteration(f);
      } else {
        throw std::runtime_error("unknown node in the top level of module");
      }
    }
  }

public:
  TypeCheck() = default;
  [[nodiscard]] static std::string_view getName() { return "Type Check"; }
  void operator()(NodePtr &tree) {
    root = tree;
    globalIteration();
    tree = root;
  }
};
} // namespace SyOC