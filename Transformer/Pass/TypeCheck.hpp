#pragma once

#include "Tree/Tree.hpp"
#include <cassert>
#include <stdexcept>

class TypeCheck {
private:
  NodePtr root;
  FunctionDeclaration *current_function;

  bool isArrayType(const Type &type) { return type.dim.size(); }

  bool isPointerLogicOp(OpType op) { return op == OP_Land || OP_Lor; }

  bool isPointerComparasionOp(OpType op) {
    return op == OP_Eq || op == OP_Ne || op == OP_Ge || op == OP_Le ||
           op == OP_Gt || op == OP_Lt;
  }

  bool isPointerArithmeticOp(OpType op) { return op == OP_Add || op == OP_Sub; }

  void checkInitializerCompatible(ExprPtr expr, const Type &type) {
    if (isArrayType(type) && !expr->is<InitListExpr *>())
      throw std::runtime_error("initializer list required");
    for (auto dim : type.dim) {
      if (!dim->is<IntegerLiteral *>())
        throw std::runtime_error(
          "array dimension must be non-negative integer");
    }
  }

  Type combine(OpType op, const Type &lhs, const Type &rhs) {
    if (!isArrayType(lhs) && !isArrayType(rhs))
      return Type{TS_Int, TQ_None, {}};
    if (isArrayType(lhs) && !isArrayType(rhs)) {
      if (isPointerArithmeticOp(op))
        return Type{TS_Int, TQ_None, lhs.dim};
      else if (isPointerComparasionOp(op) || isPointerLogicOp(op))
        return Type{TS_Int, TQ_None, {}};
      else
        throw std::runtime_error("incompatible types when lhs is an array");
    }
    if (!isArrayType(lhs) && isArrayType(rhs)) {
      if (op == OP_Add)
        return Type{TS_Int, TQ_None, lhs.dim};
      else if (isPointerComparasionOp(op) || isPointerLogicOp(op))
        return Type{TS_Int, TQ_None, {}};
      else
        throw std::runtime_error("incompatible types when rhs is an array");
    }
    // isArrayType(lhs) && isArrayType(rhs)
    if (isPointerComparasionOp(op) || isPointerLogicOp(op))
      return Type{TS_Int, TQ_None, {}};
    if (op == OP_Sub) {
      if (lhs.dim.size() != rhs.dim.size())
        throw std::runtime_error("incompatible array dimensions");
      return Type{TS_Int, TQ_None, lhs.dim};
    } else
      throw std::runtime_error(
        "unable to apply such an operator on both array types");
  }

  void checkArrayDimensionViolation(VariableDeclaration *decl) {
    if (decl->type.qual == TQ_Const && !decl->initializer)
      throw std::runtime_error(
        fmt::format("const variable {} must be initialized", decl->name));
    if (decl->initializer)
      checkInitializerCompatible(decl->initializer, decl->type);
  }

  void checkStatement(NodePtr stmt) {
    if (stmt->is<CompoundStmt *>())
      checkCompoundStmt(stmt);
    if (isVariableDeclaration(stmt))
      checkArrayDimensionViolation(stmt->as_unchecked<VariableDeclaration *>());
    if (isExpression(stmt))
      checkExpr(stmt->as_unchecked<ExprPtr>());
    if (stmt->is<IfStmt *>())
      checkIfStmt(stmt);
    if (stmt->is<WhileStmt *>())
      checkWhileStmt(stmt);
    if (stmt->is<ReturnStmt *>())
      checkReturnStmt(stmt);
  }

  void checkCompoundStmt(NodePtr stmt) {
    assert(stmt->is<CompoundStmt *>());
    for (auto child : stmt->as_unchecked<CompoundStmt *>()->stmts) 
      checkStatement(child);
  }

  void checkIfStmt(NodePtr stmt) {
    assert(stmt->is<IfStmt *>());
    auto if_stmt = stmt->as_unchecked<IfStmt *>();
    assert(isExpression(if_stmt->condition));
    auto cond_type = checkExpr(if_stmt->condition->as_unchecked<ExprPtr>());
    checkStatement(if_stmt->then_stmt);
    if (if_stmt->else_stmt)
      checkStatement(if_stmt->else_stmt);
  }

  void checkWhileStmt(NodePtr stmt) {
    assert(stmt->is<WhileStmt *>());
    auto while_stmt = stmt->as_unchecked<WhileStmt *>();
    assert(isExpression(while_stmt->condition));
    auto cond_type = checkExpr(while_stmt->condition->as_unchecked<ExprPtr>());
    checkStatement(while_stmt->body);
  }

  void checkReturnStmt(NodePtr stmt) {
    assert(stmt->is<ReturnStmt *>());
    assert(current_function);
    auto ret = stmt->as_unchecked<ReturnStmt *>();
  }

  Type checkExpr(ExprPtr expr) {
    switch (expr->node_type) {
    case ND_IntegerLiteral:
      return Type{TS_Int, TQ_None, {}};
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

  Type checkArraySubscriptExpr(ExprPtr expr) {
    assert(expr->is<ArraySubscriptExpr *>());
    auto array_subscript = expr->as_unchecked<ArraySubscriptExpr *>();
    auto array = array_subscript->array;
    auto subscript = array_subscript->subscript;
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
    return Type{array_type.spec, array_type.qual, new_dim};
  }

  Type checkUnaryExpr(ExprPtr expr) {
    assert(expr->is<UnaryExpr *>());
    auto unary = expr->as_unchecked<UnaryExpr *>();
    auto unary_type = checkExpr(unary->operand);
    if (unary_type.spec == TS_Void)
      throw std::runtime_error("cannot apply unary operator to void");
    return Type{TS_Int, TQ_None, {}};
  }

  Type checkBinaryExpr(ExprPtr expr) {
    assert(expr->is<BinaryExpr *>());
    auto binary = expr->as_unchecked<BinaryExpr *>();
    auto lhs_type = checkExpr(binary->lhs);
    auto rhs_type = checkExpr(binary->rhs);
    if (lhs_type.spec == TS_Void || rhs_type.spec == TS_Void)
      throw std::runtime_error("cannot apply binary operator to void");
    return combine(binary->op, lhs_type, rhs_type);
  }

  bool isCompatible(const Type &a, const Type &b) {
    if (a.spec == b.spec && a.dim.size() == b.dim.size())
      return true;
    return false;
  }

  Type checkCallExpr(ExprPtr expr) {
    assert(expr->is<CallExpr *>());
    auto call = expr->as_unchecked<CallExpr *>();
    auto func = call->func;
    if (!func->is<RefExpr *>())
      throw std::runtime_error(
        "cannot call non-function (function pointer is not supported)");
    auto decl = func->as_unchecked<RefExpr *>()->decl;
    if (!decl->is<FunctionDeclaration *>())
      throw std::runtime_error("cannot call non-function");
    auto func_decl = decl->as_unchecked<FunctionDeclaration *>();
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

  Type checkRefExpr(ExprPtr expr) {
    assert(expr->is<RefExpr *>());
    auto ref = expr->as_unchecked<RefExpr *>();
    auto decl = ref->decl;
    if (isVariableDeclaration(decl)) {
      return decl->as_unchecked<VariableDeclaration *>()->type;
    }
    if (decl->is<FunctionDeclaration *>()) {
      auto func = decl->as_unchecked<FunctionDeclaration *>();
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

  Type checkAssignExpr(ExprPtr expr) {
    assert(expr->is<AssignExpr *>());
    auto assign = expr->as_unchecked<AssignExpr *>();
    auto lhs_type = checkExpr(assign->lhs);
    auto rhs_type = checkExpr(assign->rhs);
    if (lhs_type.spec == TS_Void || rhs_type.spec == TS_Void)
      throw std::runtime_error("cannot assign to void or use void as value");
    if (lhs_type.qual == TQ_Const)
      throw std::runtime_error("cannot assign to const");
    return lhs_type;
  }

  void functionIteration(FunctionDeclaration *func) {
    if (func->body == nullptr)
      return;
    auto body = func->body->as<CompoundStmt *>();
    current_function = func;
    checkCompoundStmt(body);
    current_function = nullptr;
  }

  void globalIteration() {
    auto module = root->as<Module *>();
    for (auto decl : module->decls) {
      if (isVariableDeclaration(decl)) {
        auto p = decl->as_unchecked<VariableDeclaration *>();
        checkArrayDimensionViolation(p);
      } else if (isFunctionDeclaration(decl)) {
        auto f = decl->as_unchecked<FunctionDeclaration *>();
        functionIteration(f);
      } else {
        throw std::runtime_error("unknown node in the top level of module");
      }
    }
  }

public:
  TypeCheck(){};
  NodePtr operator()(NodePtr tree) {
    root = tree;
    globalIteration();
    return root;
  }
};