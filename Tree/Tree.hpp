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
#include <type_traits>
#include <unordered_map>
#include <utility>
#include <variant>
#include <vector>

namespace SyOC {
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
struct TreeExpr;
using ExprPtr = TreeExpr *;
struct TreeType;

struct TreeType {
  TypeSpecifier spec;
  TypeQualifier qual;
  std::vector<ExprPtr> dim;
  std::string toString();
  std::string dump();
  TreeType(TypeSpecifier spec, TypeQualifier qual, std::vector<ExprPtr> dim)
    : spec(spec), qual(qual), dim(std::move(dim)) {}
};

struct Node {
  NodeType node_type;
  Node(NodeType type) : node_type(type) {}
  template <typename T> bool is() {
    return node_type == std::remove_pointer_t<T>::this_type;
  }
  template <typename T> T as() {
    if (is<T>())
      return static_cast<T>(this);
    return nullptr;
  }
  template <typename T> T as_unchecked() { return static_cast<T>(this); }
};

#undef THIS
#define THIS(x) constexpr inline static NodeType this_type = x

struct TreeModule : public Node {
  THIS(ND_Module);
  std::vector<NodePtr> decls;
  TreeModule(std::vector<NodePtr> decls)
    : Node(this_type), decls(std::move(decls)) {}
};

struct TreeFunctionDeclaration : public Node {
  THIS(ND_FunctionDeclaration);
  TreeType return_type;
  std::string_view name;
  std::vector<std::pair<std::string_view, TreeType>> parameters;
  NodePtr body;
  TreeFunctionDeclaration(
    TreeType return_type, std::string_view name,
    std::vector<std::pair<std::string_view, TreeType>> parameters, NodePtr body)
    : Node(this_type), return_type(std::move(return_type)), name(name),
      parameters(std::move(parameters)), body(body) {}
};

struct TreeVariableDeclaration : public Node {
  TreeType type;
  std::string_view name;
  ExprPtr initializer;
  TreeVariableDeclaration(NodeType this_type, TreeType type,
                          std::string_view name, ExprPtr initializer)
    : Node(this_type), type(std::move(type)), name(name),
      initializer(initializer) {}
};

struct TreeGlobalDeclaration : public TreeVariableDeclaration {
  THIS(ND_GlobalDeclaration);
  TreeGlobalDeclaration(TreeType type, std::string_view name,
                        ExprPtr initializer)
    : TreeVariableDeclaration(this_type, std::move(type), name, initializer) {}
};

struct TreeLocalDeclaration : public TreeVariableDeclaration {
  THIS(ND_LocalDeclaration);
  TreeLocalDeclaration(TreeType type, std::string_view name,
                       ExprPtr initializer)
    : TreeVariableDeclaration(this_type, std::move(type), name, initializer) {}
};

struct TreeCompoundStmt : public Node {
  THIS(ND_CompoundStmt);
  std::vector<NodePtr> stmts;
  TreeCompoundStmt(std::vector<NodePtr> stmts)
    : Node(this_type), stmts(std::move(stmts)) {}
};

struct TreeIfStmt : public Node {
  THIS(ND_IfStmt);
  NodePtr condition;
  NodePtr then_stmt;
  NodePtr else_stmt;
  TreeIfStmt(NodePtr condition, NodePtr then_stmt, NodePtr else_stmt)
    : Node(this_type), condition(condition), then_stmt(then_stmt),
      else_stmt(else_stmt) {}
};

struct TreeWhileStmt : public Node {
  THIS(ND_WhileStmt);
  NodePtr condition;
  NodePtr body;
  TreeWhileStmt(NodePtr condition, NodePtr body)
    : Node(this_type), condition(condition), body(body) {}
};

struct TreeExpr : public Node {
  TreeExpr(NodeType type) : Node(type) {}
};

struct TreeInitListExpr : public TreeExpr {
  THIS(ND_InitListExpr);
  std::vector<ExprPtr> values;
  TreeInitListExpr(std::vector<ExprPtr> values)
    : TreeExpr(this_type), values(std::move(values)) {}
};

struct TreeArraySubscriptExpr : public TreeExpr {
  THIS(ND_ArraySubscriptExpr);
  ExprPtr array;
  ExprPtr subscript;
  TreeArraySubscriptExpr(ExprPtr array, ExprPtr subscript)
    : TreeExpr(this_type), array(array), subscript(subscript) {}
};

struct TreeCallExpr : public TreeExpr {
  THIS(ND_CallExpr);
  ExprPtr func;
  std::vector<ExprPtr> args;
  TreeCallExpr(ExprPtr name, std::vector<ExprPtr> args)
    : TreeExpr(this_type), func(name), args(std::move(args)) {}
};

struct TreeAssignExpr : public TreeExpr {
  THIS(ND_AssignExpr);
  ExprPtr lhs, rhs;
  TreeAssignExpr(ExprPtr lhs, ExprPtr rhs)
    : TreeExpr(this_type), lhs(lhs), rhs(rhs) {}
};

struct TreeBinaryExpr : public TreeExpr {
  THIS(ND_BinaryExpr);
  OpType op;
  ExprPtr lhs, rhs;
  TreeBinaryExpr(OpType op, ExprPtr lhs, ExprPtr rhs)
    : TreeExpr(this_type), op(op), lhs(lhs), rhs(rhs) {}
};

struct TreeUnaryExpr : public TreeExpr {
  THIS(ND_UnaryExpr);
  OpType op;
  ExprPtr operand;
  TreeUnaryExpr(OpType op, ExprPtr operand)
    : TreeExpr(this_type), op(op), operand(operand) {}
};

struct TreeIntegerLiteral : public TreeExpr {
  THIS(ND_IntegerLiteral);
  int64_t value;
  TreeIntegerLiteral(int64_t value) : TreeExpr(this_type), value(value) {}
};

struct TreeFloatLiteral : public TreeExpr {
  THIS(ND_FloatLiteral);
  float value;
  TreeFloatLiteral(float value) : TreeExpr(this_type), value(value) {}
};

struct TreeRefExpr : public TreeExpr {
  THIS(ND_RefExpr);
  std::string_view name;
  NodePtr decl;
  TreeRefExpr(std::string_view name, NodePtr decl)
    : TreeExpr(this_type), name(name), decl(decl) {}
};

struct TreeContinueStmt : public Node {
  THIS(ND_ContinueStmt);
  TreeContinueStmt(NodePtr /*parent*/) : Node(this_type) {}
};

struct TreeBreakStmt : public Node {
  THIS(ND_BreakStmt);
  TreeBreakStmt(NodePtr /*parent*/) : Node(this_type) {}
};

struct TreeReturnStmt : public Node {
  THIS(ND_ReturnStmt);
  ExprPtr value;
  TreeReturnStmt(ExprPtr value) : Node(this_type), value(value) {}
};

inline bool isVariableDeclaration(NodePtr node) {
  return node->is<TreeGlobalDeclaration *>() ||
         node->is<TreeLocalDeclaration *>();
}

inline bool isFunctionDeclaration(NodePtr node) {
  return node->is<TreeFunctionDeclaration *>();
}

inline bool isExpression(NodePtr node) {
  return node->is<TreeInitListExpr *>() ||
         node->is<TreeArraySubscriptExpr *>() || node->is<TreeCallExpr *>() ||
         node->is<TreeAssignExpr *>() || node->is<TreeBinaryExpr *>() ||
         node->is<TreeUnaryExpr *>() || node->is<TreeIntegerLiteral *>() ||
         node->is<TreeFloatLiteral *>() || node->is<TreeRefExpr *>();
  ;
}
} // namespace SyOC