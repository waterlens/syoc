#pragma once
#include "Tree/Tree.hpp"
#include <array>
#include <cstddef>
#include <cstdint>
#include <fmt/core.h>
#include <fmt/format.h>
#include <functional>
#include <limits>
#include <memory>
#include <stdexcept>
#include <string>
#include <string_view>
#include <unordered_map>
#include <utility>
#include <variant>
#include <vector>

#define OpcodeDefine(x, n) x,
enum OpType {
#include "Common/Common.def"
};

#define OpcodeDefine(x, n) n,
inline constexpr std::string_view op_name[]{
#include "Common/Common.def"
};

#define TypeSpecifierDefine(x, y) x = (y),
enum TypeSpecifier {
#include "Common/Common.def"
};

#define TypeSpecifierDefine(x, y) #x,
inline constexpr std::string_view type_spec_name[]{
#include "Common/Common.def"
};

#define TypeQualifierDefine(x, y) x = (y),
enum TypeQualifier {
#include "Common/Common.def"
};

#define TypeQualifierDefine(x, y) #x
inline constexpr std::string_view type_qual_name[]{
#include "Common/Common.def"
};

#define NodeTypeDefine(x) x,
enum NodeType {
#include "Common/Common.def"
};

struct Node;
using NodePtr = Node *;
struct Expr;
using ExprPtr = Expr *;
struct Type;

struct Type {
  TypeSpecifier spec;
  TypeQualifier qual;
  std::vector<ExprPtr> dim;
  std::string toString();
  std::string dump();
};

struct Node {
  NodeType node_type;
  Node(NodeType type) : node_type(type) {}
  template <typename T> bool is() { return node_type == T::this_type; }
};

#define THIS(x) inline static NodeType this_type = x

struct Module : public Node {
  THIS(ND_Module);
  std::vector<NodePtr> decls;
  Module(std::vector<NodePtr> decls) : Node(this_type), decls(decls) {}
};

struct GlobalDeclaration : public Node {
  THIS(ND_GlobalDeclaration);
  Type type;
  std::string_view name;
  ExprPtr initializer;
  GlobalDeclaration(Type type, std::string_view name, ExprPtr initializer)
    : Node(this_type), type(type), name(name), initializer(initializer) {}
};

struct FunctionDeclaration : public Node {
  THIS(ND_FunctionDeclaration);
  Type return_type;
  std::string_view name;
  std::vector<std::pair<std::string_view, Type>> parameters;
  NodePtr body;
  FunctionDeclaration(Type return_type, std::string_view name,
                      std::vector<std::pair<std::string_view, Type>> parameters,
                      NodePtr body)
    : Node(this_type), return_type(return_type), name(name),
      parameters(parameters), body(body) {}
};

struct VariableDeclaration : public Node {
  THIS(ND_VariableDeclaration);
  Type type;
  std::string_view name;
  NodePtr initializer;
  VariableDeclaration(Type type, std::string_view name, NodePtr initializer)
    : Node(this_type), type(type), name(name), initializer(initializer) {}
};

struct CompoundStmt : public Node {
  THIS(ND_CompoundStmt);
  std::vector<NodePtr> stmts;
  CompoundStmt(std::vector<NodePtr> stmts) : Node(this_type), stmts(stmts) {}
};

struct IfStmt : public Node {
  THIS(ND_IfStmt);
  NodePtr condition;
  NodePtr then_stmt;
  NodePtr else_stmt;
  IfStmt(NodePtr condition, NodePtr then_stmt, NodePtr else_stmt)
    : Node(this_type), condition(condition), then_stmt(then_stmt),
      else_stmt(else_stmt) {}
};

struct WhileStmt : public Node {
  THIS(ND_WhileStmt);
  NodePtr condition;
  NodePtr body;
  WhileStmt(NodePtr condition, NodePtr body)
    : Node(this_type), condition(condition), body(body) {}
};

struct Expr : public Node {
  Expr(NodeType type) : Node(type) {}
};

struct InitListExpr : public Expr {
  THIS(ND_InitListExpr);
  std::vector<ExprPtr> values;
  InitListExpr(std::vector<ExprPtr> values) : Expr(this_type), values(values) {}
};

struct ArraySubscriptExpr : public Expr {
  THIS(ND_ArraySubscriptExpr);
  ExprPtr array;
  ExprPtr subscript;
  ArraySubscriptExpr(ExprPtr array, ExprPtr subscript)
    : Expr(this_type), array(array), subscript(subscript) {}
};

struct CallExpr : public Expr {
  THIS(ND_CallExpr);
  ExprPtr name;
  std::vector<ExprPtr> args;
  CallExpr(ExprPtr name, std::vector<ExprPtr> args)
    : Expr(this_type), name(name), args(args) {}
};

struct AssignExpr : public Expr {
  THIS(ND_AssignExpr);
  ExprPtr lhs, rhs;
  AssignExpr(ExprPtr lhs, ExprPtr rhs) : Expr(this_type), lhs(lhs), rhs(rhs) {}
};

struct BinaryExpr : public Expr {
  THIS(ND_BinaryExpr);
  OpType op;
  ExprPtr lhs, rhs;
  BinaryExpr(OpType op, ExprPtr lhs, ExprPtr rhs)
    : Expr(this_type), op(op), lhs(lhs), rhs(rhs) {}
};

struct UnaryExpr : public Expr {
  THIS(ND_UnaryExpr);
  OpType op;
  ExprPtr operand;
  UnaryExpr(OpType op, ExprPtr operand)
    : Expr(this_type), op(op), operand(operand) {}
};

struct IntegerLiteral : public Expr {
  THIS(ND_IntegerLiteral);
  uint64_t value;
  IntegerLiteral(uint64_t value) : Expr(this_type), value(value) {}
};

struct RefExpr : public Expr {
  THIS(ND_RefExpr);
  std::string_view name;
  RefExpr(std::string_view name) : Expr(this_type), name(name) {}
};

struct ContinueStmt : public Node {
  THIS(ND_ContinueStmt);
  ContinueStmt(NodePtr parent) : Node(this_type) {}
};

struct BreakStmt : public Node {
  THIS(ND_BreakStmt);
  BreakStmt(NodePtr parent) : Node(this_type) {}
};

struct ReturnStmt : public Node {
  THIS(ND_ReturnStmt);
  ExprPtr value;
  ReturnStmt(ExprPtr value) : Node(this_type), value(value) {}
};