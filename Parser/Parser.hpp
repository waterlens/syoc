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
  IRBuilder builder;

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

  ValueHandle expression() { return assignmentExpression(); }

  ValueHandle assignmentExpression() {
    auto lhs = conditionalExpression();
    ValueHandle rhs = 0;
    if (consume("=")) {
      rhs = assignmentExpression();
    }
    return _dummy(lhs, rhs);
  }

  ValueHandle conditionalExpression() {
    return binaryOpExpression(0, castExpression());
  }

  ValueHandle binaryOpExpression(int prec, ValueHandle lhs) {
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

  ValueHandle castExpression() { return unaryExpression(); }

  ValueHandle unaryExpression() {
    auto next_tok = peek();
    if (next_tok.text == "-" || next_tok.text == "+" || next_tok.text == "!") {
      skip();
      return castExpression();
    }
    return postfixExpression();
  }

  ValueHandle postfixExpression() {
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

  ValueHandle argumentExpressionList() {
    vector<ValueHandle> args;
    args.push_back(assignmentExpression());
    while (consume(",")) args.push_back(assignmentExpression());
    return _dummy(0, 0);
  }

  ValueHandle primaryExpression() {
    if (consume("(")) {
      auto expr = expression();
      expect(")");
      return expr;
    }
    auto tok = peek();
    skip();
    return {};
  }

  ValueHandle _dummy(ValueHandle lhs, ValueHandle rhs) { return lhs; }

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

  ValueHandle initializerList() {
    auto init = builder.createConstantArray();
    auto &elem = builder.getConstantArray()->refElement();
    if (!peek("}")) {
      elem.emplace_back(initializer());
      while (consume(",") && !peek("}")) elem.emplace_back(initializer());
    }
    return init;
  }

  ValueHandle initializer() {
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

  ValueHandle initDeclarator(Type declspec) {
    auto [name, index, paramList, dimensions] = declarator();
    ValueHandle init;
    if (consume("=")) {
      init = initializer();
    }
    if (index == 1) {
      fmt::print("array: type: {} | name: {}\n", declspec.toString(), name);
    } else if (index == 2) {
      fmt::print("variable: type: {} | name: {}\n", declspec.toString(), name);
    }
    return init;
  }

  void externalDeclaration() {
    Type declspec = declarationSpecifiers();
    ValueHandle init;
    auto [name, index, paramList, dimensions] = declarator();
    if (index == 0) {
      compoundStatement();
      fmt::print("function: return type: {} | name: {}\n", declspec.toString(),
                 name);

      // IR generation for function
    } else {
      if (consume("=")) {
        init = initializer();
      }

      while (consume(",")) {
        initDeclarator(declspec);
      }
      expect(";");

            // IR generation for global variable
      auto gv_t = declspec;
      auto gv = builder.createGlobalVariable();
      auto p_gv = builder.getGlobalVariable();

      if (index == 1) {
        gv_t.refDimension() = dimensions;
      } else if (index == 2) {
      }
      p_gv->refType() = gv_t;
      p_gv->refName() = name;
      p_gv->refInitializer() = init;

      builder.addGlobalValue(gv);
      // IR generation end
    }
  }

  ValueHandle parameterDeclaration() {
    Type declspec = declarationSpecifiers();
    auto [name, index, paramList, dimensions] = declarator();
    if (index == 0)
      throw std::runtime_error(
        "can't use function declarator in a parameter list");
    ValueHandle var;
    return var;
  }

  vector<ValueHandle> parameterTypeList() {
    vector<ValueHandle> param;
    expect("(");
    if (!peek(")")) {
      param.push_back(parameterDeclaration());
      while (consume(",")) param.push_back(parameterDeclaration());
    }
    consume(",");
    expect(")");
    return param;
  }

  vector<ValueHandle> arrayDimmension() {
    vector<ValueHandle> dimensions;
    while (peek("[")) {
      skip();
      if (!peek("]")) {
        dimensions.push_back(assignmentExpression());
      }
      expect("]");
    }
    return dimensions;
  }

  tuple<string_view, size_t, vector<ValueHandle>, vector<ValueHandle>>
  declarator() {
    Token name = expectIdentifier();
    vector<ValueHandle> paramList, dimensions;
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
        ty.refTypeQualifier() = TQ_Const;
      else if (consume("void"))
        ty.refTypeSpecifier() = TS_Void;
      else if (consume("int"))
        ty.refTypeSpecifier() = TS_Int;
      else
        break;
    }
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

  void parse() {
    builder.createModule();
    translationUnit();
  }

  Parser(const string &input)
    : input(input), input_view(input), index(0), token_index(0) {}
};
