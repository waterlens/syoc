#pragma once
#include <cctype>
#include <cstddef>
#include <cstdlib>
#include <cstring>
#include <fmt/core.h>
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
#include <variant>
#include <vector>

#include "IR/IR.hpp"

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
        for (; i < input.length() && isxdigit(input[i]); i++)
          ;
      } else {
        for (; i < input.length() && isdigit(input[i]); i++)
          ;
      }
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

  Value *expression() { return assignmentExpression(); }

  Value *assignmentExpression() {
    auto lhs = conditionalExpression();
    Value *rhs = nullptr;
    if (consume("=")) {
      rhs = assignmentExpression();
    }
    return _dummy(lhs, rhs);
  }

  Value *conditionalExpression() {
    return binaryOpExpression(0, castExpression());
  }

  Value *binaryOpExpression(int prec, Value *lhs) {
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
      lhs = _dummy(lhs, rhs);
    }
  }

  Value *castExpression() { return unaryExpression(); }

  Value *unaryExpression() {
    auto next_tok = peek();
    if (next_tok.text == "-" || next_tok.text == "+" || next_tok.text == "!") {
      skip();
      return castExpression();
    }
    return postfixExpression();
  }

  Value *postfixExpression() {
    auto expr = primaryExpression();
    while (peek("[") || peek("("))
      if (consume("[")) {
        auto dim = expression();
        expect("]");
      } else if (consume("(")) {
        if (!peek(")"))
          auto args = argumentExpressionList();
        expect(")");
      }
    return expr;
  }

  Value *argumentExpressionList() {
    vector<Value *> args;
    args.push_back(assignmentExpression());
    while (consume(",")) args.push_back(assignmentExpression());
    return _dummy(nullptr, nullptr);
  }

  Value *primaryExpression() {
    if (consume("(")) {
      auto expr = expression();
      expect(")");
      return expr;
    }
    auto tok = peek();
    skip();
    return {};
  }

  Value *_dummy(Value *lhs, Value *rhs) { return lhs; }

  void selectionStatement() {
    expect("if");
    expect("(");
    auto cond = expression();
    expect(")");
    statement();
    if (consume("else"))
      statement();
  }

  void iterationStatement() {
    expect("while");
    expect("(");
    auto cond = expression();
    expect(")");
    statement();
  }

  void jumpStatement() {
    if (consume("continue"))
      ;
    else if (consume("break"))
      ;
    else if (consume("return")) {
      if (!peek(";"))
        auto expr = expression();
    }
    expect(";");
  }

  void expressionStatement() {
    if (!peek(";"))
      auto expr = expression();
    expect(";");
  }

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
        initDeclarator(declspec);
        while (consume(",")) initDeclarator(declspec);
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

  Value *initializerList() {
    vector<Value *> vals;
    if (!peek("}")) {
      vals.emplace_back(initializer());
      while (consume(",") && !peek("}")) vals.emplace_back(initializer());
    }
    return nullptr;
  }

  Value *initializer() {
    if (consume("{")) {
      auto initList = initializerList();
      consume(",");
      expect("}");
      return initList;
    } else {
      auto expr = assignmentExpression();
      return expr;
    }
  }

  void initDeclarator(Type declspec) {
    auto [name, index, paramList, dimensions] = declarator();
    Value *init;
    if (consume("=")) {
      init = initializer();
    }
    if (index == 1) {
      fmt::print("array: type: {} | name: {}\n", declspec.toString(), name);
    } else if (index == 2) {
      fmt::print("index_t: type: {} | name: {}\n", declspec.toString(), name);
    }
  }

  void externalDeclaration() {
    Type declspec = declarationSpecifiers();
    Value *init;
    auto [name, index, paramList, dimensions] = declarator();
    if (index == 0) {
      compoundStatement();
      fmt::print("function: return type: {} | name: {}\n", declspec.toString(),
                 name);
    } else {
      if (consume("=")) {
        init = initializer();
      }
      if (index == 1) {
        fmt::print("array: type: {} | name: {}\n", declspec.toString(), name);
      } else if (index == 2) {
        fmt::print("index_t: type: {} | name: {}\n", declspec.toString(),
                   name);
      }
      while (consume(",")) {
        initDeclarator(declspec);
      }
      expect(";");
    }
  }

  index_t parameterDeclaration() {
    Type declspec = declarationSpecifiers();
    auto [name, index, paramList, dimensions] = declarator();
    if (index == 0)
      throw std::runtime_error(
        "can't use function declarator in a parameter list");
    index_t var;
    return var;
  }

  vector<index_t> parameterTypeList() {
    vector<index_t> param;
    expect("(");
    if (!peek(")")) {
      param.push_back(parameterDeclaration());
      while (consume(",")) param.push_back(parameterDeclaration());
    }
    consume(",");
    expect(")");
    return param;
  }

  vector<Value> arrayDimmension() {
    vector<Value> dimensions;
    while (peek("[")) {
      skip();
      if (!peek("]")) {
        assignmentExpression();
      }
      expect("]");
    }
    return dimensions;
  }

  tuple<string_view, size_t, vector<index_t>, vector<Value>> declarator() {
    Token name = expectIdentifier();
    vector<index_t> paramList;
    vector<Value> dimensions;
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
    /*
    for (;;) {
      if (consume("const"))
        ty.ty_qual |= CONST;
      else if (consume("void"))
        ty.ty_spec += VOID;
      else if (consume("int"))
        ty.ty_spec += INT;
      else
        break;
    }*/
    return ty;
  }

  void translationUnit() {
    for (;;) {
      auto tok = peek();
      if (tok.token_type == TokenType::EndOfFile)
        break;
      externalDeclaration();
    }
  }

  void parse() { translationUnit(); }

  Parser(const string &input)
    : input(input), input_view(input), index(0), token_index(0) {}
};
