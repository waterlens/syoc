#pragma once

#include "IR/IR.hpp"
#include "Tree/Tree.hpp"
#include "Util/TrivialValueVector.hpp"

#include <cassert>
#include <numeric>
#include <stdexcept>

class Tree2SSA {
  using TypeHandle = std::pair<SSAType, SSAValueHandle>;
  NodePtr root;
  IRHost *host;
  std::pair<BasicBlock *, SSAValueHandle> global_initializer_block;
  Scope<SSAValueHandle> scopes;

  void setupGlobalInitializerFunction() {
    auto &host_ref = *host;
    auto [init, handle1] = host_ref.createFunction();
    auto [bb, handle2] = host_ref.createBasicBlock(handle1);

    init->name = "__syoc@init";
    init->args = {};
    init->return_type = SSAType{SSAType::PrimitiveType::Void, 0, {}};
    init->basic_block.push_back(handle2);

    global_initializer_block = {bb, handle2};
  }

  SSAType convertType(const Type &ty) {
    SSAType ssa_ty{};
    if (ty.spec == TS_Int) {
      ssa_ty.primitive_type = SSAType::PrimitiveType::Integer;
      ssa_ty.width = 32;
    } else if (ty.spec == TS_Void) {
      ssa_ty.primitive_type = SSAType::PrimitiveType::Void;
      ssa_ty.width = 0;
    }
    for (auto &&dim : ty.dim) {
      ssa_ty.dimension.push_back(dim->as<IntegerLiteral *>()->value);
    }
    return ssa_ty;
  }

  SSAType convertParameterType(const Type &ty) {
    SSAType ssa_ty{};
    if (ty.spec == TS_Int)
      ssa_ty.primitive_type = SSAType::PrimitiveType::Integer;
    else if (ty.spec == TS_Void)
      ssa_ty.primitive_type = SSAType::PrimitiveType::Void;
    for (auto &&dim : ty.dim) {
      ssa_ty.dimension.push_back(dim->as<IntegerLiteral *>()->value);
    }
    // array name decay
    if (ssa_ty.dimension.size()) {
      ssa_ty.dimension.pop_front();
      ssa_ty.indirect_level++;
    }
    return ssa_ty;
  }

  size_t calculateArrayTotalLength(SSAType ty) {
    return std::accumulate(
      ty.dimension.begin(), ty.dimension.end(),
      ty.width / CHAR_BIT *
        (ty.primitive_type == SSAType::PrimitiveType::Void ? 0 : 1),
      [](auto lhs, auto rhs) { return lhs * rhs; });
  }

  TypeHandle generateLValue(ExprPtr expr) {
    auto &host_ref = *host;
    switch (expr->node_type) {
    case ND_ArraySubscriptExpr: {
      auto arr_sub = expr->as_unchecked<ArraySubscriptExpr *>();
      auto [arr_ty, arr_handle] = generateLValue(arr_sub->array);
      auto [idx_ty, idx_handle] = generateRValue(arr_sub->subscript);
      assert(arr_ty.indirect_level && arr_ty.dimension.size());
      auto new_ty = arr_ty;
      new_ty.dimension.pop_back();
      auto [elem, elem_handle] =
        host_ref.createInstruction(OP_Offset, new_ty, arr_handle, idx_handle);
      return {new_ty, elem_handle};
    }
    case ND_RefExpr: {
      auto ref = expr->as_unchecked<RefExpr *>();
      auto handle = scopes.find(ref->name);
      if (handle == 0)
        throw std::runtime_error(
          fmt::format("undefined variable: {}", ref->name));
      auto &value = host_ref[handle];

      return {};
    }
    case ND_AssignExpr: {
      auto assign = expr->as_unchecked<AssignExpr *>();
      auto [lhs_ty, lhs_handle] = generateLValue(assign->lhs);
      auto [rhs_ty, rhs_handle] = generateRValue(assign->rhs);

      auto [assign_insn, insn_handle] =
        host_ref.createInstruction(OP_Store, lhs_ty, lhs_handle, rhs_handle);

      return {lhs_ty, insn_handle};
    }
    default:
      throw std::runtime_error("not a supported expression");
    }
  }

  TypeHandle generateRValue(ExprPtr expr) {
    auto &host_ref = *host;
    switch (expr->node_type) {
    case ND_IntegerLiteral: {
      auto [lit, lit_handle] = host_ref.createConstantInteger(
        expr->as_unchecked<IntegerLiteral *>()->value);
      return {IntType, lit_handle};
    }
    case ND_UnaryExpr: {
      auto unary = expr->as_unchecked<UnaryExpr *>();
      auto [operand_ty, operand_handle] = generateRValue(unary->operand);
      if (unary->op == OP_Neg) {
        auto [_, insn_handle] =
          host_ref.createInstruction(OP_Neg, operand_ty, operand_handle);
        return {operand_ty, insn_handle};
      }
      if (unary->op == OP_LNot) {
        auto [_, insn_handle] =
          host_ref.createInstruction(OP_LNot, IntType, operand_handle);
        return {IntType, insn_handle};
      }
      throw std::runtime_error("not a supported unary operator");
    }
    case ND_BinaryExpr: {
      auto binary = expr->as_unchecked<BinaryExpr *>();
      auto [lhs_ty, lhs_handle] = generateRValue(binary->lhs);
      auto [rhs_ty, rhs_handle] = generateRValue(binary->rhs);
      assert(lhs_ty.dimension.empty() && rhs_ty.dimension.empty());
      auto [_, insn_handle] =
        host_ref.createInstruction(binary->op, IntType, lhs_handle, rhs_handle);
      return {IntType, insn_handle};
    }
    case ND_CallExpr: {
      auto call = expr->as_unchecked<CallExpr *>();
      auto func_ast_expr = call->func->as_unchecked<FunctionDeclaration *>();
      auto func_name = func_ast_expr->name;

      auto return_ty = convertType(func_ast_expr->return_type);
      // TODO: need get the handle function
      // need some cast for arguments here ?
      TrivialValueVector<SSAValueHandle, 3> args;
      for (auto &&arg : call->args) {
        auto [arg_ty, arg_handle] = generateRValue(arg);
        args.push_back(arg_handle);
      }
      auto [call_insn, call_handle] =
        host_ref.createInstruction(OP_Call, return_ty);
      call_insn->args = args;
      return {return_ty, call_handle};
    }
    case ND_ArraySubscriptExpr:
    case ND_RefExpr:
    case ND_AssignExpr: {
      auto [lv_ty, lv_handle] = generateLValue(expr);
      assert(lv_ty.indirect_level);
      assert(lv_ty.dimension.empty());
      auto new_ty = lv_ty;
      new_ty.indirect_level--;
      auto [_, insn_handle] =
        host_ref.createInstruction(OP_Load, new_ty, lv_handle);
      return TypeHandle{new_ty, insn_handle};
    }
    default:
      throw std::runtime_error("not a supported expression");
    }
  }

  void generateInitializer(ExprPtr init, SSAType ty, SSAValueHandle target) {
    auto &host_ref = *host;
    host_ref.setInsertPoint(global_initializer_block);

    assert(ty.indirect_level);

    auto len = calculateArrayTotalLength(ty);
    auto [arr_len, len_handle] = host_ref.createConstantInteger(len);

    if (ty.dimension.size()) {
      auto [set_inst, set_handle] =
        host_ref.createIntrinsic("__syoc@memset0", VoidType);
      set_inst->args = {target, len_handle};
    }

    if (init) {
      if (init->is<InitListExpr *>()) {
        auto l = init->as_unchecked<InitListExpr *>();
        size_t i = 0;
        auto bound = ty.dimension.size() ? ty.dimension[0] : 1;
        auto elem_ty = ty;
        elem_ty.dimension.pop_front();
        for (auto &&e : l->values) {
          if (i >= bound)
            break;
          auto [offset_n, offset_handle] = host_ref.createConstantInteger(i);
          auto [elem, elem_handle] = host_ref.createInstruction(
            OP_Offset, elem_ty, target, offset_handle);
          generateInitializer(e, elem_ty, elem_handle);
          i++;
        }
      } else {
        auto [rv_ty, rv_handle] = generateRValue(init);
        auto [init_insn, insn_handle] =
          host_ref.createInstruction(OP_Store, ty, target, rv_handle);
      }
    }
  }

  void generateGlobalVariable(GlobalDeclaration *decl) {
    auto ty = convertType(decl->type);
    ty.indirect_level++;
    auto [g, handle] = host->createGlobalVariable();
    g->type = ty;
    g->name = decl->name;
    generateInitializer(decl->initializer, ty, handle);
    scopes.insert(decl->name, handle);
  }

  void globalGeneration() {
    setupGlobalInitializerFunction();
    auto module = root->as<Module *>();
    scopes.entry();
    for (auto decl : module->decls) {
      if (decl->is<GlobalDeclaration *>()) {
        auto p = decl->as_unchecked<GlobalDeclaration *>();
        generateGlobalVariable(p);
      } else if (isFunctionDeclaration(decl)) {
        auto f = decl->as_unchecked<FunctionDeclaration *>();
        functionGeneration(f);
      }
    }
    scopes.exit();
  }

  void generateStatement(CompoundStmt *stmt) {}

  void functionGeneration(FunctionDeclaration *decl) {
    auto &host_ref = *host;
    auto [f, f_handle] = host_ref.createFunction();
    f->return_type = convertType(decl->return_type);
    f->name = decl->name;

    scopes.insert(decl->name, f_handle);

    bool is_external = decl->body == nullptr;
    f->external = is_external;

    if (!is_external) {
      auto entry_bb = host_ref.createBasicBlock(f_handle);
      host_ref.setInsertPoint(entry_bb);
      f->basic_block.push_back(entry_bb.second);
    }

    scopes.entry();
    for (auto &&param : decl->parameters) {
      auto [arg, arg_handle] = host_ref.createArgument(f_handle);
      arg->name = param.first;
      arg->type = convertParameterType(param.second);
      f->args.push_back(arg_handle);

      if (!is_external) {
        auto addr_ty = arg->type;
        addr_ty.indirect_level += 1;
        auto [arg_addr, addr_handle] =
          host_ref.createInstruction(OP_Allocate, addr_ty);

        auto [store, store_handle] = host_ref.createInstruction(
          OP_Store, addr_ty, addr_handle, arg_handle);
        scopes.insert(arg->name, addr_handle);
      }
    }
    if (!is_external) {
      scopes.entry();
      generateStatement(decl->body->as<CompoundStmt *>());
      scopes.exit();
    }
    scopes.exit();
  }

public:
  Tree2SSA(){};
  IRHost *operator()(NodePtr tree) {
    host = new IRHost();
    root = tree;
    globalGeneration();
    return host;
  }
};