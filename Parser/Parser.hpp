#pragma once
#include <cctype>
#include <cstddef>
#include <cstdlib>
#include <cstring>
#include <fmt/core.h>
#include <fmt/format.h>
#include <functional>
#include <iterator>
#include <stdexcept>
#include <string>
#include <string_view>
#include <tuple>
#include <type_traits>
#include <unordered_set>
#include <variant>
#include <vector>

#include "IR/Constant.hpp"
#include "IR/Scope.hpp"
#include "IR/Type.hpp"
#include "IR/Variable.hpp"

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
  size_t index;
  vector<Token> tokens;
  size_t token_index;
  Scope function_scope, variable_scope;

  inline static unordered_set<string_view> keywords = {
    "break", "const",  "continue", "else",  "if",
    "int",   "return", "void",     "while",
  };
  inline static string_view long_operators[] = {
    "<=", ">=", "==", "!=", "&&", "||"};
  inline static string_view operators = "()[]{}<>+-*/%!;,=";

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
      for (; i < input.length() && isdigit(input[i]); ++i)
        ;
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
        tokens.emplace_back(
          keywords.count(id) ? TokenType::Keyword : TokenType::Identifier, id);
      } else if ((len = delimitOperator())) {
        index += len;
        tokens.emplace_back(TokenType::Operator, input_view.substr(start, len));
      } else if ((len = delimitIntegerConstant())) {
        index += len;
        tokens.emplace_back(TokenType::IntegerConstant,
                            input_view.substr(start, len));
      } else if (index != input.length())
        throw runtime_error(
          fmt::format("unexpected token {} ...", input_view.substr(start, 16)));
    }

    tokens.emplace_back(TokenType::EndOfFile, string_view("@EOF"));
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

  void selectionStatement() {}

  void iterationStatement() {}

  void jumpStatement() {}

  void expressionStatement() {}

  void statement() {
    if (peek("{"))
      compoundStatement();
    else if (peek("if"))
      selectionStatement();
    else if (peek("while"))
      iterationStatement();
    else if (peek("break") || peek("continue") || peek("return"))
      jumpStatement();
    else
      expressionStatement();
  }

  void blockItem() {
    if (peek("void") || peek("int") || peek("const")) {
      Type declspec = declarationSpecifiers();
      if (!peek(";")) {
        initDeclarator<false>(declspec);
        while (consume(",")) initDeclarator<false>(declspec);
      }
      expect(";");
    } else
      statement();
  }

  void compoundStatement() {
    expect("{");
    while (!peek("}")) blockItem();
    expect("}");
  }

  template <bool constEval> Value initializerList() {
    vector<Value> vals;
    if (!peek("}")) {
      vals.emplace_back(initializer<constEval>());
      while (consume(",") && !peek("}"))
        vals.emplace_back(initializer<constEval>());
    }
    return {vals};
  }

  template <bool constEval> Value initializer() {
    if (consume("{")) {
      auto initList = initializerList<true>();
      consume(",");
      expect("}");
      return initList;
    } else if constexpr (constEval) {
      auto i = constEvalAssignExpr();
      return {i};
    } else {
      // TODO: assignment expression
      return {};
    }
  }

  template <bool constEval> void initDeclarator(Type t) {
    auto [name, index, paramList, dimensions] = declarator();
    if (consume("=")) {
      auto init = initializer<constEval>();
    }
  }

  void externalDeclaration() {
    Type declspec = declarationSpecifiers();
    auto [name, index, paramList, dimensions] = declarator();
    if (index == 0) {
      compoundStatement();
    } else {
      if (consume("=")) {
        auto init = initializer<true>();
      }
      while (consume(",")) {
        initDeclarator<true>(declspec);
      }
      expect(";");
    }
  }

  Variable parameterDeclaration() {
    Type declspec = declarationSpecifiers();
    auto [name, index, paramList, dimensions] = declarator();
    if (index == 0)
      throw std::runtime_error(
        "can't use function declarator in a parameter list");
    Variable var;
    return Variable{declspec, name, dimensions};
  }

  vector<Variable> parameterTypeList() {
    vector<Variable> param;
    expect("(");
    if (!peek(")")) {
      param.push_back(parameterDeclaration());
      while (consume(",")) param.push_back(parameterDeclaration());
    }
    consume(",");
    expect(")");
    return param;
  }

  vector<IntegerConstant> arrayDimmension() {
    vector<IntegerConstant> dimensions;
    while (peek("[")) {
      skip();
      dimensions.push_back(constEvalAssignExpr());
      expect("]");
    }
    return dimensions;
  }

  tuple<string_view, size_t, vector<Variable>, vector<IntegerConstant>>
  declarator() {
    Token name = expectIdentifier();
    vector<Variable> paramList;
    vector<IntegerConstant> dimensions;
    size_t index;
    if (peek("(")) {
      index = 0;
      paramList = parameterTypeList();
    } else if (peek("[")) {
      index = 1;
      dimensions = arrayDimmension();
    } else {
      index = 2;
    }

    return {name.text, index, paramList, dimensions};
  }

  Type declarationSpecifiers() {
    Type ty{};
    for (;;) {
      if (consume("const"))
        ty.ty_qual |= CONST;
      else if (consume("void"))
        ty.ty_spec += VOID;
      else if (consume("int"))
        ty.ty_spec += INT;
      else
        break;
    }
    return ty;
  }

  void translationUnit() {}

  IntegerConstant constEvalAssignExpr() { return constEvalLogicalOrExpr(); }

  IntegerConstant constEvalLogicalOrExpr() {
    auto v1 = constEvalLogicalAndExpr();
    for (;;) {
      if (consume("||")) {
        auto v2 = constEvalLogicalAndExpr();
        v1 = {v1.literal || v2.literal};
      } else
        break;
    }
    return v1;
  }

  IntegerConstant constEvalLogicalAndExpr() {
    auto v1 = constEvalEqualityExpr();
    for (;;) {
      if (consume("&&")) {
        auto v2 = constEvalEqualityExpr();
        v1 = {v1.literal && v2.literal};
      } else
        break;
    }
    return v1;
  }

  IntegerConstant constEvalEqualityExpr() {
    auto v1 = constEvalRelExpr();
    for (;;) {
      if (consume("==")) {
        auto v2 = constEvalRelExpr();
        v1 = {v1.literal == v2.literal};
      } else if (consume("!=")) {
        auto v2 = constEvalRelExpr();
        v1 = {v1.literal != v2.literal};
      } else
        break;
    }
    return v1;
  }

  IntegerConstant constEvalRelExpr() {
    auto v1 = constEvalAddExpr();
    for (;;) {
      if (consume("<")) {
        auto v2 = constEvalAddExpr();
        v1 = {v1.literal < v2.literal};
      } else if (consume(">")) {
        auto v2 = constEvalAddExpr();
        v1 = {v1.literal > v2.literal};
      } else if (consume("<=")) {
        auto v2 = constEvalAddExpr();
        v1 = {v1.literal <= v2.literal};
      } else if (consume(">=")) {
        auto v2 = constEvalAddExpr();
        v1 = {v1.literal >= v2.literal};
      } else
        break;
    }
    return v1;
  }

  IntegerConstant constEvalAddExpr() {
    auto v1 = constEvalMulExpr();
    for (;;) {
      if (consume("+")) {
        auto v2 = constEvalMulExpr();
        v1 = {v1.literal + v2.literal};
      } else if (consume("-")) {
        auto v2 = constEvalMulExpr();
        v1 = {v1.literal - v2.literal};
      } else
        break;
    }
    return v1;
  }

  IntegerConstant constEvalMulExpr() {
    auto v1 = constEvalCastExpr();
    for (;;) {
      if (consume("*")) {
        auto v2 = constEvalCastExpr();
        v1 = {v1.literal * v2.literal};
      } else if (consume("/")) {
        auto v2 = constEvalCastExpr();
        if (v2.literal == 0)
          throw runtime_error("division by 0");
        v1 = {v1.literal / v2.literal};
      } else if (consume("%")) {
        auto v2 = constEvalCastExpr();
        if (v2.literal == 0)
          throw runtime_error("division by 0");
        v1 = {v1.literal % v2.literal};
      } else
        break;
    }
    return v1;
  }

  IntegerConstant constEvalCastExpr() { return constEvalUnaryExpr(); }

  IntegerConstant constEvalUnaryExpr() {
    if (consume("+"))
      return constEvalCastExpr();
    if (consume("-"))
      return {-constEvalCastExpr().literal};
    if (consume("!"))
      return {!constEvalRelExpr().literal};
    return constEvalPostfixExpr();
  }

  IntegerConstant constEvalPostfixExpr() {
    auto i = constEvalPrimaryExpr();
    if (peek("[") || peek("("))
      throw runtime_error("initializer element is not constant");
    return i;
  }

  IntegerConstant constEvalPrimaryExpr() {
    if (consume("(")) {
      auto i = constEvalExpr();
      expect(")");
      return i;
    } else {
      auto [is_ident, ident] = peekIdentifier();
      if (is_ident) {
        skip();
        if (variable_scope.table.count(ident.text)) {
          auto val = variable_scope.table.find(ident.text)->second;
          if (auto int_const_p = get_if<IntegerConstant>(&val.v)) {
            return *int_const_p;
          }
        }
        throw runtime_error(fmt::format("{} is not a constant", ident.text));
      } else {
        auto [is_int_const, int_const] = peekIntegerConstant();
        if (is_int_const) {
          skip();
          int i = strtol(int_const.text.data(), nullptr, 10);
          return {i};
        }
      }
    }
    throw runtime_error("initializer element is not constant");
  }

  IntegerConstant constEvalExpr() {
    auto i = constEvalAssignExpr();
    if (peek(","))
      throw runtime_error("initializer element is not constant");
    return i;
  }

  Parser(const string &input)
    : input(input), input_view(input), index(0), token_index(0) {}
};
