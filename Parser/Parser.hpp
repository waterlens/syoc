#pragma once
#include <cassert>
#include <cctype>
#include <cstddef>
#include <cstdint>
#include <cstdlib>
#include <cstring>
#include <fmt/format.h>
#include <functional>
#include <iterator>
#include <limits>
#include <memory>
#include <stdexcept>
#include <string>
#include <string_view>
#include <tuple>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#include "Tree/Tree.hpp"
#include "Util/Scope.hpp"

namespace SyOC {
enum class TokenType {
  IntegerConstant,
  FloatConstant,
  Keyword,
  Identifier,
  Operator,
  EndOfFile,
};

struct Token {
  TokenType token_type;
  std::string_view text;
  unsigned line_pos;
};

struct Parser {
  std::string input;
  std::string_view input_view;
  std::vector<Token> tokens;
  size_t index{};
  size_t token_index{};
  Scope<NodePtr> scope;

  inline static std::unordered_set<std::string_view> keywords = {
    "break", "const",  "continue", "else",  "if",
    "int",   "return", "void",     "while", "float",
  };
  inline static std::string_view long_operators[] = {
    "<=", ">=", "==", "!=", "&&", "||"};
  inline static std::string_view operators = "()[]{}<>+-*/%!;,=";

  inline static std::unordered_map<std::string_view, int> bin_op_precedence = {
    {"*", 120}, {"/", 120}, {"%", 120}, {"+", 110}, {"-", 110},
    {"<", 90},  {">", 90},  {"<=", 90}, {">=", 90}, {"==", 60},
    {"!=", 80}, {"&&", 30}, {"||", 30},
  };

  inline static std::unordered_map<std::string_view, OpType> bin_op_code = {
    {"*", OP_Mul},  {"/", OP_Div}, {"%", OP_Mod}, {"+", OP_Add},
    {"-", OP_Sub},  {"<", OP_Lt},  {">", OP_Gt},  {"<=", OP_Le},
    {">=", OP_Ge},  {"==", OP_Eq}, {"!=", OP_Ne}, {"&&", OP_Land},
    {"||", OP_Lor},
  };

  unsigned skipWhitespace() {
    unsigned new_lines = 0;
    for (; index < input.size() && isspace(input[index]) != 0; index++) {
      if ((index + 1 < input.size() && input[index] == '\r' &&
           input[index + 1] == '\n') ||
          input[index] == '\r' || input[index] == '\n')
        new_lines++;
    }
    return new_lines;
  }

  bool startWith(std::string_view s) {
    return input.size() - index >= s.length() &&
           memcmp(s.data(), input.data() + index, s.length()) == 0;
  }

  size_t delimitIdentifier() {
    if (index < input.length())
      if (isalpha(input[index]) != 0 || input[index] == '_') {
        auto i = index + 1;
        for (; i < input.length() &&
               ((isalnum(input[i]) != 0 || input[i] == '_'));
             ++i)
          ;
        return i - index;
      }
    return 0;
  }

  size_t delimitOperator() {
    for (auto &&s : long_operators)
      if (startWith(s))
        return s.length();
    if (index < input.length() &&
        memchr(operators.data(), input[index], operators.length()) != nullptr)
      return 1;
    return 0;
  }

  size_t delimitIntegerConstant() {
    if (index < input.length()) {
      char *p_end = nullptr;
      strtol(input.data() + index, &p_end, 0);
      return p_end - input.data() - index;
    }
    return 0;
  }

  size_t delimitFloatConstant() {
    if (index < input.length()) {
      char *p_end = nullptr;
      strtof(input.data() + index, &p_end);
      return p_end - input.data() - index;
    }
    return 0;
  }

  void skipLineComment() {
    index += 2;
    while (index < input.length() && input[index] != '\n') index++;
  }

  unsigned skipBlockComment() {
    unsigned new_lines = 0;
    bool found = false;
    index += 2;

    for (; index < input.size(); index++) {
      if ((index + 1 < input.size() && input[index] == '\r' &&
           input[index + 1] == '\n') ||
          input[index] == '\r' || input[index] == '\n')
        new_lines++;
      if (index + 1 < input.size() && input[index] == '*' &&
          input[index + 1] == '/') {
        found = true;
        break;
      }
    }

    if (!found)
      throw std::runtime_error("block comment doesn't have a close tag");

    index += 2;
    return new_lines;
  }

  void tokenize() {
    unsigned lineno = 1;
    while (index != input.length()) {
      lineno += skipWhitespace();
      auto start = index;
      size_t len;
      size_t len2;

      if (startWith("//")) {
        skipLineComment();
      } else if (startWith("/*")) {
        lineno += skipBlockComment();
      } else if ((len = delimitIdentifier()) != 0U) {
        index += len;
        auto id = input_view.substr(start, len);
        tokens.push_back({keywords.count(id) != 0U ? TokenType::Keyword
                                                   : TokenType::Identifier,
                          id, lineno});
      } else if ((len = delimitOperator()) != 0U) {
        index += len;
        tokens.push_back(
          {TokenType::Operator, input_view.substr(start, len), lineno});
      } else if ((len = delimitFloatConstant()) +
                   (len2 = delimitIntegerConstant()) !=
                 0U) {
        // NOTICE: don't short circuit here
        if (len2 >= len) {
          index += len2;
          tokens.push_back({TokenType::IntegerConstant,
                            input_view.substr(start, len2), lineno});
        } else {
          index += len;
          tokens.push_back(
            {TokenType::FloatConstant, input_view.substr(start, len), lineno});
        }
      } else if (index != input.length())
        throw std::runtime_error(
          fmt::format("unexpected token {} ...", input_view.substr(start, 16)));
    }

    tokens.push_back({TokenType::EndOfFile, std::string_view("@EOF"), lineno});
  }

  bool peek(std::string_view s) {
    return token_index < tokens.size() && tokens[token_index].text == s;
  }

  void skip() { token_index++; }

  void expect(std::string_view s) {
    if (peek(s)) {
      skip();
      return;
    }
    throw std::runtime_error(
      fmt::format("expect {} but got {}", s, tokens[token_index].text));
  }

  bool consume(std::string_view s) {
    if (peek(s)) {
      skip();
      return true;
    }
    return false;
  }

  Token peek() { return tokens[token_index]; }

  Token expectIdentifier() {
    if (token_index < tokens.size() &&
        tokens[token_index].token_type == TokenType::Identifier) {
      return tokens[token_index++];
    }
    throw std::runtime_error(
      fmt::format("expect identifier but got {}", tokens[token_index].text));
  }

  Token expectIntegerConstant() {
    if (token_index < tokens.size() &&
        tokens[token_index].token_type == TokenType::IntegerConstant) {
      return tokens[token_index++];
    }
    throw std::runtime_error(fmt::format("expect integer constant but got {}",
                                         tokens[token_index].text));
  }

  Token expectFloatConstant() {
    if (token_index < tokens.size() &&
        tokens[token_index].token_type == TokenType::FloatConstant) {
      return tokens[token_index++];
    }
    throw std::runtime_error(fmt::format("expect float constant but got {}",
                                         tokens[token_index].text));
  }

  std::tuple<bool, Token> peekIdentifier() {
    if (token_index < tokens.size() &&
        tokens[token_index].token_type == TokenType::Identifier) {
      return {true, tokens[token_index]};
    }
    return {false, {}};
  }

  std::tuple<bool, Token> peekIntegerConstant() {
    if (token_index < tokens.size() &&
        tokens[token_index].token_type == TokenType::IntegerConstant) {
      return {true, tokens[token_index]};
    }
    return {false, {}};
  }

  ExprPtr expression() { return assignmentExpression(); }

  ExprPtr assignmentExpression() {
    auto *lhs = conditionalExpression();
    ExprPtr rhs = nullptr;
    if (consume("="))
      rhs = assignmentExpression();
    if (rhs != nullptr)
      return new TreeAssignExpr{lhs, rhs};
    return lhs;
  }

  ExprPtr conditionalExpression() {
    return binaryOpExpression(0, castExpression());
  }

  ExprPtr binaryOpExpression(int prec, ExprPtr lhs) {
    for (;;) {
      auto tok = peek();
      int tok_prec;
      int next_prec;
      if (bin_op_precedence.count(tok.text) == 0U)
        tok_prec = -1;
      else
        tok_prec = bin_op_precedence.find(tok.text)->second;
      if (tok_prec < prec)
        return lhs;
      skip();
      auto *rhs = castExpression();
      auto next_tok = peek();
      if (bin_op_precedence.count(next_tok.text) == 0U)
        next_prec = -1;
      else
        next_prec = bin_op_precedence.find(next_tok.text)->second;
      if (tok_prec < next_prec)
        rhs = binaryOpExpression(tok_prec + 1, rhs);
      if (bin_op_code.count(tok.text) == 0U)
        throw std::runtime_error(
          fmt::format("unknown binary operator {}", tok.text));
      lhs = new TreeBinaryExpr{bin_op_code.find(tok.text)->second, lhs, rhs};
    }
    return lhs;
  }

  ExprPtr castExpression() { return unaryExpression(); }

  ExprPtr unaryExpression() {
    auto next_tok = peek();
    if (next_tok.text == "+") {
      skip();
      return castExpression();
    }
    if (next_tok.text == "-" || next_tok.text == "!") {
      skip();
      OpType op = next_tok.text == "-"   ? OP_Neg
                  : next_tok.text == "!" ? OP_Lnot
                                         : OP_End;
      return new TreeUnaryExpr{op, unaryExpression()};
    }
    return postfixExpression();
  }

  ExprPtr postfixExpression() {
    auto *expr = primaryExpression();
    while (peek("[") || peek("("))
      if (consume("[")) {
        auto *dim = expression();
        expect("]");
        expr = new TreeArraySubscriptExpr{expr, dim};
      } else if (peek("(")) {
        auto args = argumentExpressionList();
        expr = new TreeCallExpr{expr, args};
      }
    return expr;
  }

  std::vector<ExprPtr> argumentExpressionList() {
    std::vector<ExprPtr> args;
    expect("(");
    if (!peek(")")) {
      args.push_back(assignmentExpression());
      while (consume(",")) args.push_back(assignmentExpression());
    }
    expect(")");
    return args;
  }

  ExprPtr primaryExpression() {
    static std::string convert_buffer;
    if (consume("(")) {
      auto *expr = expression();
      expect(")");
      return expr;
    }
    auto tok = peek();
    skip();

    if (tok.token_type == TokenType::IntegerConstant) {
      convert_buffer = tok.text;
      return new TreeIntegerLiteral{
        std::strtoll(convert_buffer.c_str(), nullptr, 0)};
    }

    if (tok.token_type == TokenType::FloatConstant) {
      convert_buffer = tok.text;
      return new TreeFloatLiteral{std::strtof(convert_buffer.c_str(), nullptr)};
    }

    if (tok.token_type == TokenType::Identifier) {
      auto *node = scope.find(tok.text);
      if (node == nullptr)
        throw std::runtime_error(fmt::format("undefined symbol {}", tok.text));
      return new TreeRefExpr{tok.text, node};
    }

    throw std::runtime_error(fmt::format("unexpected token {}", tok.text));
  }

  NodePtr selectionStatement() {
    NodePtr cond;
    NodePtr then_stmt;
    NodePtr else_stmt;
    expect("if");
    expect("(");
    cond = expression();
    expect(")");
    then_stmt = statement();
    if (consume("else"))
      else_stmt = statement();
    else
      else_stmt = nullptr;
    return new TreeIfStmt{cond, then_stmt, else_stmt};
  }

  NodePtr iterationStatement() {
    NodePtr cond;
    NodePtr body_stmt;
    expect("while");
    expect("(");
    cond = expression();
    expect(")");
    body_stmt = statement();

    return new TreeWhileStmt{cond, body_stmt};
  }

  NodePtr jumpStatement() {
    NodePtr stmt;
    if (consume("continue"))
      stmt = new TreeContinueStmt{nullptr};
    else if (consume("break"))
      stmt = new TreeBreakStmt{nullptr};
    else if (consume("return")) {
      ExprPtr expr = nullptr;
      if (!peek(";"))
        expr = expression();
      stmt = new TreeReturnStmt{expr};
    }
    expect(";");
    return stmt;
  }

  NodePtr expressionStatement() {
    NodePtr expr = nullptr;
    if (!peek(";")) {
      expr = expression();
    }
    expect(";");
    return expr != nullptr ? expr : new TreeCompoundStmt{{}};
  }

  NodePtr statement() {
    NodePtr stmt;
    if (peek("{"))
      stmt = compoundStatement();
    else if (peek("if"))
      stmt = selectionStatement();
    else if (peek("while"))
      stmt = iterationStatement();
    else if (peek("break") || peek("continue") || peek("return"))
      stmt = jumpStatement();
    else
      stmt = expressionStatement();
    return stmt;
  }

  void blockItem(std::vector<NodePtr> &stmts) {
    if (peek("void") || peek("int") || peek("const") || peek("float")) {
      auto declspec = declarationSpecifiers();

      if (!peek(";")) {
        stmts.emplace_back(initDeclarator(declspec, false));
        while (consume(","))
          stmts.emplace_back(initDeclarator(declspec, false));
      }
      expect(";");
    } else if (auto *stmt = statement())
      stmts.emplace_back(stmt);
  }

  NodePtr compoundStatement() {
    scope.enter();
    auto *stmt = new TreeCompoundStmt{{}};
    expect("{");
    while (!peek("}")) blockItem(stmt->stmts);
    expect("}");
    scope.exit();
    return stmt;
  }

  std::vector<ExprPtr> initializerList() {
    std::vector<ExprPtr> elem;
    if (!peek("}")) {
      elem.emplace_back(initializer());
      while (consume(",") && !peek("}")) elem.emplace_back(initializer());
    }
    return elem;
  }

  ExprPtr initializer() {
    if (consume("{")) {
      auto initList = initializerList();
      consume(",");
      expect("}");
      return new TreeInitListExpr{initList};
    }
    auto *expr = assignmentExpression();
    return expr;
  }

  std::vector<NodePtr> externalDeclaration() {
    auto declspec = declarationSpecifiers();
    ExprPtr init = nullptr;
    std::vector<NodePtr> all_decls;
    auto [name, index, param_list, dimensions] = declarator();
    if (index == 0) {
      auto *func =
        new TreeFunctionDeclaration{declspec, name, param_list, nullptr};
      scope.insert(name, func);
      all_decls.emplace_back(func);
      if (peek("{")) { // function definition
        scope.enter();
        for (auto &param : param_list) scope.insert(param.first, func);
        auto *stmt = compoundStatement();
        scope.exit();
        func->body = stmt;
      } else {
        while (consume(",")) {
          all_decls.emplace_back(initDeclarator(declspec, true));
        }
        expect(";");
      }
    } else {
      if (consume("="))
        init = initializer();
      auto ty = TreeType{declspec.spec, declspec.qual,
                         index == 1 ? dimensions : std::vector<ExprPtr>{}};

      all_decls.emplace_back(new TreeGlobalDeclaration{ty, name, init});
      scope.insert(name, all_decls.back());

      while (consume(",")) {
        all_decls.emplace_back(initDeclarator(declspec, true));
      }
      expect(";");
    }
    return all_decls;
  }

  NodePtr initDeclarator(const TreeType &declspec, bool is_global) {
    auto [name, index, param_list, dimensions] = declarator();
    ExprPtr init = nullptr;
    if (consume("="))
      init = initializer();

    if (index == 0) {
      auto *func =
        new TreeFunctionDeclaration{declspec, name, param_list, nullptr};
      scope.insert(name, func);
      scope.enter();
      for (auto &param : param_list) scope.insert(param.first, func);
      scope.exit();
      return func;
    }

    auto ty = TreeType{declspec.spec, declspec.qual,
                       index == 1 ? dimensions : std::vector<ExprPtr>{}};
    auto *decl = is_global ? (new TreeGlobalDeclaration{ty, name, init})
                               ->as_unchecked<TreeVariableDeclaration *>()
                           : (new TreeLocalDeclaration{ty, name, init})
                               ->as_unchecked<TreeVariableDeclaration *>();
    scope.insert(name, decl);
    return decl;
  }

  std::pair<std::string_view, TreeType> parameterDeclaration() {
    TreeType declspec = declarationSpecifiers();
    auto [name, index, param_list, dimensions] = declarator();
    if (index == 0)
      throw std::runtime_error(
        "can't use function declarator in a parameter list");
    auto ty = TreeType{declspec.spec, declspec.qual,
                       index == 1 ? dimensions : std::vector<ExprPtr>{}};
    return {name, ty};
  }

  std::vector<std::pair<std::string_view, TreeType>> parameterTypeList() {
    std::vector<std::pair<std::string_view, TreeType>> param;
    expect("(");
    if (!peek(")")) {
      param.emplace_back(parameterDeclaration());
      while (consume(",")) param.emplace_back(parameterDeclaration());
    }
    consume(",");
    expect(")");
    static std::unordered_set<std::string_view> param_names;
    param_names.clear();
    for (auto &p : param) {
      if (param_names.find(p.first) != param_names.end())
        throw std::runtime_error("duplicate parameter name");
      param_names.insert(p.first);
    }
    return param;
  }

  std::vector<ExprPtr> arrayDimension() {
    std::vector<ExprPtr> dimensions;
    while (peek("[")) {
      skip();
      if (peek("]"))
        dimensions.emplace_back(
          new TreeIntegerLiteral{std::numeric_limits<int64_t>::max()});
      else
        dimensions.push_back(assignmentExpression());

      expect("]");
    }
    return dimensions;
  }

  std::tuple<std::string_view, size_t,
             std::vector<std::pair<std::string_view, TreeType>>,
             std::vector<ExprPtr>>
  declarator() {
    Token name = expectIdentifier();
    std::vector<std::pair<std::string_view, TreeType>> param_list;
    std::vector<ExprPtr> dimensions;
    size_t index;
    if (peek("(")) {
      index = 0;
      param_list = parameterTypeList();
    } else if (peek("[")) {
      index = 1;
      dimensions = arrayDimension();
    } else {
      index = 2;
    }

    return {name.text, index, param_list, dimensions};
  }

  TreeType declarationSpecifiers() {
    TypeQualifier qual = TQ_None;
    TypeSpecifier spec = TS_None;

    for (;;) {
      if (consume("const"))
        qual = TQ_Const;
      else if (consume("void"))
        spec = TS_Void;
      else if (consume("int"))
        spec = TS_Int;
      else if (consume("float"))
        spec = TS_Float;
      else
        break;
    }

    if (spec == TS_None)
      throw std::runtime_error("expect type specifier");

    return TreeType{spec, qual, std::vector<ExprPtr>{}};
  }

  NodePtr translationUnit() {
    std::vector<NodePtr> decls;
    scope.enter();
    for (;;) {
      auto tok = peek();
      if (tok.token_type == TokenType::EndOfFile)
        break;
      auto decl = externalDeclaration();
      for (auto &e : decl) decls.emplace_back(e);
    }
    scope.exit();
    return new TreeModule{decls};
  }

  NodePtr parse() { return translationUnit(); }

  Parser(const std::string &input) : input(input), input_view(input) {}
};
} // namespace SyOC
