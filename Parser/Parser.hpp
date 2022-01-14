#pragma once
#include <cassert>
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
#include <utility>
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
  vector<unordered_map<string_view, ValueHandle>> scopes;

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

  ValueHandle expression(bool is_constant_expr) { return assignmentExpression(is_constant_expr); }

  ValueHandle assignmentExpression(bool is_constant_expr) {
    auto lhs = conditionalExpression(is_constant_expr);
    ValueHandle rhs = 0;
    if (consume("=")) {
      rhs = assignmentExpression(is_constant_expr);
    }
    return _dummy(lhs, rhs);
  }

  ValueHandle conditionalExpression(bool is_constant_expr) {
    return binaryOpExpression(0, castExpression(is_constant_expr), is_constant_expr);
  }

  ValueHandle binaryOpExpression(int prec, ValueHandle lhs,
                                 bool is_constant_expr) {
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
      auto rhs = castExpression(is_constant_expr);
      auto next_tok = peek();
      if (!bin_op_precedence.count(next_tok.text))
        next_prec = -1;
      else
        next_prec = bin_op_precedence.find(next_tok.text)->second;
      if (tok_prec < next_prec)
        rhs = binaryOpExpression(tok_prec + 1, rhs, is_constant_expr);
      assert(bin_op_code.count(tok.text));
      if (is_constant_expr)
        lhs = get<0>(builder.createConstantExpr(
          bin_op_code.find(tok.text)->second, lhs, rhs));
      else
        lhs = get<0>(builder.createInstruction(
          bin_op_code.find(tok.text)->second, lhs, rhs));
    }
    return lhs;
  }

  ValueHandle castExpression(bool is_constant_expr) {
    return unaryExpression(is_constant_expr);
  }

  ValueHandle unaryExpression(bool is_constant_expr) {
    auto next_tok = peek();
    if (next_tok.text == "+") {
      skip();
      return castExpression(is_constant_expr);
    } else if (next_tok.text == "-" || next_tok.text == "!") {
      skip();
      OpType op = next_tok.text == "-"   ? OP_Neg
                  : next_tok.text == "!" ? OP_LNot
                                         : OP_End;
      if (is_constant_expr)
        return get<0>(builder.createConstantExpr(
          op, castExpression(is_constant_expr), invalid_value_handle));
      return get<0>(builder.createInstruction(
        op, castExpression(is_constant_expr), invalid_value_handle));
    }
    return postfixExpression(is_constant_expr);
  }

  ValueHandle postfixExpression(bool is_constant_expr) {
    auto expr = primaryExpression(is_constant_expr);
    while (peek("[") || peek("("))
      if (consume("[")) {
        auto dim = expression(is_constant_expr);
        expect("]");
      } else if (consume("(")) {
        if (!peek(")"))
          auto args = argumentExpressionList(is_constant_expr);
        expect(")");
      }
    return expr;
  }

  ValueHandle argumentExpressionList(bool is_constant_expr) {
    vector<ValueHandle> args;
    args.push_back(assignmentExpression(false));
    while (consume(",")) args.push_back(assignmentExpression(is_constant_expr));
    return _dummy(0, 0);
  }

  ValueHandle primaryExpression(bool is_constant_expr) {
    if (consume("(")) {
      auto expr = expression(is_constant_expr);
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
    auto cond = expression(false);
    expect(")");
    statement();
    if (consume("else"))
      statement();
  }

  void iterationStatement() {
    expect("while");
    expect("(");
    auto cond = expression(false);
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
        auto expr = expression(false);
    }
    expect(";");
  }

  void expressionStatement() {
    if (!peek(";"))
      auto expr = expression(false);
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
        initDeclarator(declspec, false);
        while (consume(",")) initDeclarator(declspec, false);
      }
      expect(";");
    } else
      statement();
  }

  vector<ValueHandle> compoundStatement() {
    scopes.emplace_back();
    expect("{");
    while (!peek("}")) blockItem();
    expect("}");
    scopes.pop_back();
    return {};
  }

  ValueHandle initializerList(bool is_constant_expr) {
    auto [arr, p_arr] = builder.createConstantArray();
    auto &elem = p_arr->refElement();
    if (!peek("}")) {
      elem.emplace_back(initializer(is_constant_expr));
      while (consume(",") && !peek("}"))
        elem.emplace_back(initializer(is_constant_expr));
    }
    return arr;
  }

  ValueHandle initializer(bool is_constant_expr) {
    if (consume("{")) {
      auto initList = initializerList(is_constant_expr);
      consume(",");
      expect("}");
      return initList;
    } else {
      auto expr = assignmentExpression(is_constant_expr);
      return expr;
    }
  }

  void generateGlobalVariable(Type declspec, string_view name,
                              ValueHandle init) {
    auto [gv, p_gv] = builder.createGlobalVariable();

    p_gv->refType() = declspec;
    p_gv->refName() = name;
    p_gv->refInitializer() = init;

    builder.addGlobalValue(gv);

    scopes.back()[name] = gv;
  }

  void externalDeclaration() {
    Type declspec = declarationSpecifiers();
    ValueHandle init;
    auto [name, index, param_list, dimensions] = declarator();

    if (index == 0) {
      auto [f, p_f] = builder.createFunction();
      auto [bb, p_bb] = builder.createBasicBlock();
      p_f->refName() = name;
      p_f->refArgumentList() = param_list;
      p_f->refReturnType() = declspec;
      p_f->refBasicBlock().emplace_back(bb);
      p_f->refBasicBlock() = compoundStatement();

      scopes.back()[name] = f;
    } else {
      if (consume("=")) {
        init = initializer(true);
      }

      auto ty = declspec;
      if (index == 1)
        ty.refDimension() = dimensions;
      generateGlobalVariable(ty, name, init);

      while (consume(",")) {
        initDeclarator(declspec, true);
      }
      expect(";");
    }
  }

  void initDeclarator(Type declspec, bool is_global) {
    auto [name, index, param_list, dimensions] = declarator();
    ValueHandle init;
    if (consume("=")) {
      init = initializer(is_global);
    }
    if (index == 0)
      throw std::runtime_error("can't use function declarator here");

    auto ty = declspec;
    if (index == 1)
      ty.refDimension() = dimensions;
    if (is_global) {
      generateGlobalVariable(ty, name, init);
    } else {
      // generate local variable
      auto [inst, p_inst] = builder.createInstruction(
        OP_Allocate, invalid_value_handle, invalid_value_handle);
      p_inst->refType() = ty;
      scopes.back()[name] = inst;
    }
  }

  ValueHandle parameterDeclaration() {
    Type declspec = declarationSpecifiers();
    auto [name, index, param_list, dimensions] = declarator();
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
        dimensions.push_back(assignmentExpression(true));
      }
      expect("]");
    }
    return dimensions;
  }

  tuple<string_view, size_t, vector<ValueHandle>, vector<ValueHandle>>
  declarator() {
    Token name = expectIdentifier();
    vector<ValueHandle> param_list, dimensions;
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
    scopes.emplace_back();
    translationUnit();
    scopes.pop_back();
    builder.dumpAll();
  }

  Parser(const string &input)
    : input(input), input_view(input), index(0), token_index(0) {}
};
