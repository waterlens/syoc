#pragma once
#include <cassert>
#include <cctype>
#include <cstddef>
#include <cstdlib>
#include <cstring>
#include <fmt/format.h>
#include <functional>
#include <iterator>
#include <memory>
#include <stdexcept>
#include <string>
#include <string_view>
#include <tuple>
#include <type_traits>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <variant>
#include <vector>

#include "Tree/Tree.hpp"

using namespace std;

enum class TokenType {
  IntegerConstant,
  Keyword,
  Identifier,
  Operator,
  EndOfFile,
};

struct Token {
  TokenType token_type;
  string_view text;
};

struct Parser {
  string input;
  string_view input_view;
  vector<Token> tokens;
  size_t index;
  size_t token_index;
  Scope<NodePtr> scope;

  inline static unordered_set<string_view> keywords = {
    "break", "const",  "continue", "else",  "if",
    "int",   "return", "void",     "while",
  };
  inline static string_view long_operators[] = {
    "<=", ">=", "==", "!=", "&&", "||"};
  inline static string_view operators = "()[]{}<>+-*/%!;,=";

  inline static unordered_map<string_view, int> bin_op_precedence = {
    {"*", 30},  {"/", 30},   {"%", 30},   {"+", 40},  {"-", 40},
    {"<", 60},  {">", 60},   {"<=", 60},  {">=", 60}, {"==", 70},
    {"!=", 70}, {"&&", 110}, {"||", 120},
  };

  inline static unordered_map<string_view, OpType> bin_op_code = {
    {"*", OP_Mul},  {"/", OP_Div}, {"%", OP_Mod}, {"+", OP_Add},
    {"-", OP_Sub},  {"<", OP_Lt},  {">", OP_Gt},  {"<=", OP_Le},
    {">=", OP_Ge},  {"==", OP_Eq}, {"!=", OP_Ne}, {"&&", OP_Land},
    {"||", OP_Lor},
  };

  void skipWhitespace() {
    for (; index < input.size() && isspace(input[index]); index++)
      ;
  }

  bool startWith(string_view s) {
    return input.size() - index >= s.length() &&
           memcmp(s.data(), input.data() + index, s.length()) == 0;
  }

  size_t delimitIdentifier() {
    if (index < input.length())
      if (isalpha(input[index]) || input[index] == '_') {
        auto i = index + 1;
        for (; i < input.length() && ((isalnum(input[i]) || input[i] == '_'));
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
      auto i = index;
      if (startWith("0x")) {
        i += 2;
        while (i < input.length() && isxdigit(input[i])) i++;
      } else
        while (i < input.length() && isdigit(input[i])) i++;
      return i - index;
    }
    return 0;
  }

  void skipLineComment() {
    index += 2;
    while (index < input.length() && input[index] != '\n') index++;
  }

  void skipBlockComment() {
    index += 2;
    auto begin = input.c_str() + index;
    auto closed = strstr(begin, "*/");
    if (!closed)
      throw runtime_error("block comment doesn't have a close tag");
    index += closed - begin;
    index += 2;
  }

  void tokenize() {
    while (index != input.length()) {
      skipWhitespace();
      auto start = index;
      size_t len;

      if (startWith("//")) {
        skipLineComment();
      } else if (startWith("/*")) {
        skipBlockComment();
      } else if ((len = delimitIdentifier())) {
        index += len;
        auto id = input_view.substr(start, len);
        tokens.push_back(
          {keywords.count(id) ? TokenType::Keyword : TokenType::Identifier,
           id});
      } else if ((len = delimitOperator())) {
        index += len;
        tokens.push_back({TokenType::Operator, input_view.substr(start, len)});
      } else if ((len = delimitIntegerConstant())) {
        index += len;
        tokens.push_back(
          {TokenType::IntegerConstant, input_view.substr(start, len)});
      } else if (index != input.length())
        throw runtime_error(
          fmt::format("unexpected token {} ...", input_view.substr(start, 16)));
    }

    tokens.push_back({TokenType::EndOfFile, string_view("@EOF")});
  }

  bool peek(string_view s) {
    return token_index < tokens.size() && tokens[token_index].text == s;
  }

  void skip() { token_index++; }

  void expect(string_view s) {
    if (peek(s)) {
      skip();
      return;
    }
    throw runtime_error(
      fmt::format("expect {} but got {}", s, tokens[token_index].text));
  }

  bool consume(string_view s) {
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
    throw runtime_error(
      fmt::format("expect identifier but got {}", tokens[token_index].text));
  }

  Token expectIntegerConstant() {
    if (token_index < tokens.size() &&
        tokens[token_index].token_type == TokenType::IntegerConstant) {
      return tokens[token_index++];
    }
    throw runtime_error(fmt::format("expect integer constant but got {}",
                                    tokens[token_index].text));
  }

  tuple<bool, Token> peekIdentifier() {
    if (token_index < tokens.size() &&
        tokens[token_index].token_type == TokenType::Identifier) {
      return {true, tokens[token_index]};
    }
    return {false, {}};
  }

  tuple<bool, Token> peekIntegerConstant() {
    if (token_index < tokens.size() &&
        tokens[token_index].token_type == TokenType::IntegerConstant) {
      return {true, tokens[token_index]};
    }
    return {false, {}};
  }

  ExprPtr expression() { return assignmentExpression(); }

  ExprPtr assignmentExpression() {
    auto lhs = conditionalExpression();
    ExprPtr rhs = nullptr;
    if (consume("="))
      rhs = assignmentExpression();
    if (rhs)
      return new AssignExpr{lhs, rhs};
    return lhs;
  }

  ExprPtr conditionalExpression() {
    return binaryOpExpression(0, castExpression());
  }

  ExprPtr binaryOpExpression(int prec, ExprPtr lhs) {
    for (;;) {
      auto tok = peek();
      int tok_prec, next_prec;
      if (!bin_op_precedence.count(tok.text))
        tok_prec = -1;
      else
        tok_prec = bin_op_precedence.find(tok.text)->second;
      if (tok_prec < prec)
        return lhs;
      skip();
      auto rhs = castExpression();
      auto next_tok = peek();
      if (!bin_op_precedence.count(next_tok.text))
        next_prec = -1;
      else
        next_prec = bin_op_precedence.find(next_tok.text)->second;
      if (tok_prec < next_prec)
        rhs = binaryOpExpression(tok_prec + 1, rhs);
      if (!bin_op_code.count(tok.text))
        throw std::runtime_error(
          fmt::format("unknown binary operator {}", tok.text));
      lhs = new BinaryExpr{bin_op_code.find(tok.text)->second, lhs, rhs};
    }
    return lhs;
  }

  ExprPtr castExpression() { return unaryExpression(); }

  ExprPtr unaryExpression() {
    auto next_tok = peek();
    if (next_tok.text == "+") {
      skip();
      return castExpression();
    } else if (next_tok.text == "-" || next_tok.text == "!") {
      skip();
      OpType op = next_tok.text == "-"   ? OP_Neg
                  : next_tok.text == "!" ? OP_LNot
                                         : OP_End;
      return new UnaryExpr{op, unaryExpression()};
    }
    return postfixExpression();
  }

  ExprPtr postfixExpression() {
    auto expr = primaryExpression();
    while (peek("[") || peek("("))
      if (consume("[")) {
        auto dim = expression();
        expect("]");
        expr = new ArraySubscriptExpr{expr, dim};
      } else if (peek("(")) {
        auto args = argumentExpressionList();
        expr = new CallExpr{expr, args};
      }
    return expr;
  }

  vector<ExprPtr> argumentExpressionList() {
    vector<ExprPtr> args;
    expect("(");
    if (!peek(")")) {
      args.push_back(assignmentExpression());
      while (consume(",")) args.push_back(assignmentExpression());
    }
    expect(")");
    return args;
  }

  ExprPtr primaryExpression() {
    static string convert_buffer;
    if (consume("(")) {
      auto expr = expression();
      expect(")");
      return expr;
    }
    auto tok = peek();
    skip();
    if (tok.token_type == TokenType::IntegerConstant) {
      convert_buffer = tok.text;
      unsigned radix = convert_buffer.starts_with("0x") ? 16 : 10;
      return new IntegerLiteral{
        std::strtoull(convert_buffer.c_str(), nullptr, radix)};
    } else if (tok.token_type == TokenType::Identifier) {
      auto node = scope.find(tok.text);
      if (!node)
        throw std::runtime_error(fmt::format("undefined symbol {}", tok.text));
      return new RefExpr{tok.text, node};
    }
    throw std::runtime_error(fmt::format("unexpected token {}", tok.text));
  }

  NodePtr selectionStatement() {
    NodePtr cond, then_stmt, else_stmt = nullptr;
    expect("if");
    expect("(");
    cond = expression();
    expect(")");
    then_stmt = statement();
    if (consume("else")) {
      else_stmt = statement();
    }
    return new IfStmt{cond, then_stmt, else_stmt};
  }

  NodePtr iterationStatement() {
    NodePtr cond, body_stmt = nullptr;
    expect("while");
    expect("(");
    cond = expression();
    expect(")");
    body_stmt = statement();

    return new WhileStmt{cond, body_stmt};
  }

  NodePtr jumpStatement() {
    NodePtr stmt;
    if (consume("continue"))
      stmt = new ContinueStmt{nullptr};
    else if (consume("break"))
      stmt = new BreakStmt{nullptr};
    else if (consume("return")) {
      ExprPtr expr = nullptr;
      if (!peek(";"))
        expr = expression();
      stmt = new ReturnStmt{expr};
    }
    expect(";");
    return stmt;
  }

  NodePtr expressionStatement() {
    NodePtr expr = nullptr;
    if (!peek(";")) {
      auto expr = expression();
    }
    expect(";");
    return expr;
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

  void blockItem(vector<NodePtr> &stmts) {
    if (peek("void") || peek("int") || peek("const")) {
      auto declspec = declarationSpecifiers();

      if (!peek(";")) {
        stmts.emplace_back(initDeclarator(declspec, false));
        while (consume(","))
          stmts.emplace_back(initDeclarator(declspec, false));
      }
      expect(";");
    } else if (auto stmt = statement())
      stmts.emplace_back(stmt);
  }

  NodePtr compoundStatement() {
    scope.entry();
    auto stmt = new CompoundStmt{{}};
    expect("{");
    while (!peek("}")) blockItem(stmt->stmts);
    expect("}");
    scope.exit();
    return stmt;
  }

  vector<ExprPtr> initializerList() {
    vector<ExprPtr> elem;
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
      return new InitListExpr{initList};
    } else {
      auto expr = assignmentExpression();
      return expr;
    }
  }

  vector<NodePtr> externalDeclaration() {
    auto declspec = declarationSpecifiers();
    ExprPtr init = nullptr;
    vector<NodePtr> all_decls;
    auto [name, index, param_list, dimensions] = declarator();
    if (index == 0) {
      auto func = new FunctionDeclaration{declspec, name, param_list, nullptr};
      scope.insert(name, func);
      scope.entry();
      for (auto &param : param_list) scope.insert(param.first, func);
      auto stmt = compoundStatement();
      scope.exit();
      func->body = stmt;
      all_decls.emplace_back(func);
    } else {
      if (consume("="))
        init = initializer();
      auto ty = Type{declspec.spec, declspec.qual,
                     index == 1 ? dimensions : vector<ExprPtr>{}};

      all_decls.emplace_back(new GlobalDeclaration{ty, name, init});
      scope.insert(name, all_decls.back());

      while (consume(",")) {
        all_decls.emplace_back(initDeclarator(declspec, true));
      }
      expect(";");
    }
    return all_decls;
  }

  NodePtr initDeclarator(Type declspec, bool is_global) {
    auto [name, index, param_list, dimensions] = declarator();
    ExprPtr init = nullptr;
    if (consume("="))
      init = initializer();

    if (index == 0)
      throw std::runtime_error("can't use function declarator here");

    auto ty = Type{declspec.spec, declspec.qual,
                   index == 1 ? dimensions : vector<ExprPtr>{}};
    auto *decl = is_global ? (new GlobalDeclaration{ty, name, init})
                               ->cast_unchecked<VariableDeclaration *>()
                           : (new LocalDeclaration{ty, name, init})
                               ->cast_unchecked<VariableDeclaration *>();
    scope.insert(name, decl);
    return decl;
  }

  pair<string_view, Type> parameterDeclaration() {
    Type declspec = declarationSpecifiers();
    auto [name, index, param_list, dimensions] = declarator();
    if (index == 0)
      throw std::runtime_error(
        "can't use function declarator in a parameter list");
    auto ty = Type{declspec.spec, declspec.qual,
                   index == 1 ? dimensions : vector<ExprPtr>{}};
    return {name, ty};
  }

  vector<pair<string_view, Type>> parameterTypeList() {
    vector<pair<string_view, Type>> param;
    expect("(");
    if (!peek(")")) {
      param.emplace_back(parameterDeclaration());
      while (consume(",")) param.emplace_back(parameterDeclaration());
    }
    consume(",");
    expect(")");
    return param;
  }

  vector<ExprPtr> arrayDimmension() {
    vector<ExprPtr> dimensions;
    while (peek("[")) {
      skip();
      if (!peek("]")) {
        dimensions.push_back(assignmentExpression());
      }
      expect("]");
    }
    return dimensions;
  }

  tuple<string_view, size_t, vector<pair<string_view, Type>>, vector<ExprPtr>>
  declarator() {
    Token name = expectIdentifier();
    vector<pair<string_view, Type>> param_list;
    vector<ExprPtr> dimensions;
    size_t index;
    if (peek("(")) {
      index = 0;
      param_list = parameterTypeList();
    } else if (peek("[")) {
      index = 1;
      dimensions = arrayDimmension();
    } else {
      index = 2;
    }

    return {name.text, index, param_list, dimensions};
  }

  Type declarationSpecifiers() {
    TypeQualifier qual = TQ_None;
    TypeSpecifier spec = TS_None;

    for (;;) {
      if (consume("const"))
        qual = TQ_Const;
      else if (consume("void"))
        spec = TS_Void;
      else if (consume("int"))
        spec = TS_Int;
      else
        break;
    }

    if (spec == TS_None)
      throw std::runtime_error("expect type specifier");

    return Type{spec, qual, vector<ExprPtr>{}};
  }

  NodePtr translationUnit() {
    vector<NodePtr> decls;
    scope.entry();
    auto fake_func = new FunctionDeclaration{
      Type{TS_Void, TQ_None, {}}, "_dummy_", {}, nullptr};
    scope.insert("getint", fake_func);
    scope.insert("getch", fake_func);
    scope.insert("getarray", fake_func);
    scope.insert("putint", fake_func);
    scope.insert("putch", fake_func);
    scope.insert("putarray", fake_func);
    scope.insert("starttime", fake_func);
    scope.insert("stoptime", fake_func);
    for (;;) {
      auto tok = peek();
      if (tok.token_type == TokenType::EndOfFile)
        break;
      auto decl = externalDeclaration();
      for (auto &e : decl) decls.emplace_back(e);
    }
    scope.exit();
    return new Module{decls};
  }

  NodePtr parse() { return translationUnit(); }

  Parser(const string &input)
    : input(input), input_view(input), index(0), token_index(0) {}
};
