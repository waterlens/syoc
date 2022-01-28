#pragma once

#include "IR/IR.hpp"
#include "Tree/Tree.hpp"
#include "Util/TrivialValueVector.hpp"

#include <cassert>
#include <cstddef>
#include <numeric>
#include <stdexcept>
#include <tuple>
#include <vector>

class Tree2SSA {
  using TypeHandle = std::pair<SSAType, SSAValue &>;
  NodePtr root;
  IRHost *host;
  BasicBlock *global_initializer_block, *current_return_bb;
  Function *current_function;
  Instruction *current_retval;
  Scope<SSAValueHandle> scopes;
  std::vector<BasicBlock *> bb_break, bb_continue;

  void setupGlobalInitializerFunction() {
    auto &host_ref = *host;
    auto init = host_ref.createFunction();
    auto bb = host_ref.createBasicBlock(*init);

    init->name = "__syoc@init";
    init->args = {};
    init->return_type = SSAType{SSAType::PrimitiveType::Void, 0, {}};
    init->basic_block.push_back(*bb);
    init->external = false;

    global_initializer_block = bb;
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
    if (auto arr_sub = expr->as<ArraySubscriptExpr *>()) {
      auto [arr_ty, arr_handle] = generateLValue(arr_sub->array);
      auto [idx_ty, idx_handle] = generateRValue(arr_sub->subscript);
      assert(arr_ty.indirect_level + arr_ty.dimension.size());
      auto new_ty = arr_ty;
      if (new_ty.dimension.size())
        new_ty.dimension.pop_front();
      else
        new_ty.indirect_level--;
      auto elem = host_ref.createInstruction(
        OP_Offset, new_ty, {arr_handle, host_ref.Zero, idx_handle});
      return {new_ty, *elem};
    } else if (auto ref = expr->as<RefExpr *>()) {
      auto handle = scopes.find(ref->name);
      if (handle == 0)
        throw std::runtime_error(
          fmt::format("undefined variable: {}", ref->name));
      auto &value = host_ref[handle];
      SSAType ty;
      if (value.is<Instruction *>() &&
          value.as_unchecked<Instruction *>()->op == OP_Allocate) {
        ty = value.as_unchecked<Instruction *>()->type;
      } else if (value.is<GlobalVariable *>()) {
        ty = value.as_unchecked<GlobalVariable *>()->type;
      } else
        throw std::runtime_error("name is not variable");
      return TypeHandle{ty, value};
    } else if (auto assign = expr->as<AssignExpr *>()) {
      auto [lhs_ty, lhs_handle] = generateLValue(assign->lhs);
      auto [rhs_ty, rhs_handle] = generateRValue(assign->rhs);

      auto &&assign_insn =
        host_ref.createInstruction(OP_Store, lhs_ty, {lhs_handle, rhs_handle});

      return {lhs_ty, lhs_handle};
    } else
      throw std::runtime_error("not a supported expression");
  }

  TypeHandle generateShortCircuit(BinaryExpr *binary) {
    auto &host_ref = *host;
    auto [lhs_ty, lhs] = generateRValue(binary->lhs);
    auto tmp_addr_ty = lhs_ty;
    tmp_addr_ty.indirect_level++;

    auto tmp_var_addr = host_ref.createInstruction(OP_Allocate, tmp_addr_ty);
    host_ref.createInstruction(OP_Store, tmp_addr_ty, {*tmp_var_addr, lhs});

    auto rhs_eval_bb = host_ref.createBasicBlock(*current_function);
    auto next_bb = host_ref.createBasicBlock(*current_function);
    auto branch = host_ref.createInstruction(OP_Br, IntType, {lhs});

    if (binary->op == OP_Land) {
      branch->args.push_back(*rhs_eval_bb);
      branch->args.push_back(*next_bb);
    } else {
      branch->args.push_back(*next_bb);
      branch->args.push_back(*rhs_eval_bb);
    }

    host_ref.setInsertPoint(rhs_eval_bb);
    auto [rhs_ty, rhs] = generateRValue(binary->rhs);
    host_ref.createInstruction(OP_Store, tmp_addr_ty, {*tmp_var_addr, rhs});
    host_ref.createInstruction(OP_Jump, IntType, {*next_bb});

    host_ref.setInsertPoint(next_bb);
    auto final = host_ref.createInstruction(OP_Load, lhs_ty, {*tmp_var_addr});

    current_function->basic_block.push_back(*rhs_eval_bb);
    current_function->basic_block.push_back(*next_bb);

    return {lhs_ty, *final};
  }

  TypeHandle generateRValue(ExprPtr expr) {
    auto &host_ref = *host;
    switch (expr->node_type) {
    case ND_IntegerLiteral: {
      auto lit = host_ref.createConstantInteger(
        expr->as_unchecked<IntegerLiteral *>()->value);
      return {IntType, *lit};
    }
    case ND_UnaryExpr: {
      auto unary = expr->as_unchecked<UnaryExpr *>();
      auto [operand_ty, operand_handle] = generateRValue(unary->operand);
      if (unary->op == OP_Neg) {
        return {operand_ty, *host_ref.createInstruction(OP_Neg, operand_ty,
                                                        {operand_handle})};
      }
      if (unary->op == OP_Lnot) {
        return {IntType, *host_ref.createInstruction(OP_Lnot, IntType,
                                                     {operand_handle})};
      }
      throw std::runtime_error("not a supported unary operator");
    }
    case ND_BinaryExpr: {
      auto binary = expr->as_unchecked<BinaryExpr *>();
      if (binary->op == OP_Land || binary->op == OP_Lor) {
        return generateShortCircuit(binary);
      } else {
        auto [lhs_ty, lhs] = generateRValue(binary->lhs);
        auto [rhs_ty, rhs] = generateRValue(binary->rhs);
        assert(lhs_ty.dimension.empty() && rhs_ty.dimension.empty());
        return {lhs_ty,
                *host_ref.createInstruction(binary->op, lhs_ty, {lhs, rhs})};
      }
    }
    case ND_CallExpr: {
      auto call = expr->as_unchecked<CallExpr *>();
      auto name = call->func->as_unchecked<RefExpr *>()->name;
      auto handle = scopes.find(name);
      if (!handle.isValid() || handle.id == 0)
        throw std::runtime_error("can't found function");
      auto f = host_ref[handle].as<Function *>();
      TrivialValueVector<SSAValueHandle, 3> args;
      for (auto &&arg : call->args) {
        auto [arg_ty, arg_handle] = generateRValue(arg);
        args.push_back(arg_handle);
      }
      auto call_insn =
        host_ref.createInstruction(OP_Call, f->return_type, {handle});
      for (auto &&arg : args) call_insn->args.push_back(arg);
      return {f->return_type, *call_insn};
    }
    case ND_ArraySubscriptExpr:
    case ND_RefExpr:
    case ND_AssignExpr: {
      auto [lv_ty, lv_handle] = generateLValue(expr);
      assert(lv_ty.indirect_level + lv_ty.dimension.size());
      auto new_ty = lv_ty;
      new_ty.indirect_level--;
      return TypeHandle{
        new_ty, *host_ref.createInstruction(OP_Load, new_ty, {lv_handle})};
    }
    default:
      throw std::runtime_error("not a supported expression");
    }
  }

  void generateInitializer(ExprPtr init, SSAType ty, SSAValueHandle target,
                           bool memset0 = true) {
    auto &host_ref = *host;

    assert(ty.indirect_level + ty.dimension.size());

    auto len = calculateArrayTotalLength(ty);
    auto len_constant = host_ref.createConstantInteger(len);

    if (memset0 && ty.dimension.size()) {
      auto set_insn = host_ref.createInstruction(OP_Memset0, VoidType);
      set_insn->args = {target, *len_constant};
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
          auto offset_n = host_ref.createConstantInteger(i);
          auto elem = host_ref.createInstruction(
            OP_Offset, elem_ty, {target, host_ref.Zero, *offset_n});
          generateInitializer(e, elem_ty, *elem, false);
          i++;
        }
      } else {
        auto [rv_ty, rv_handle] = generateRValue(init);
        auto final_ty = ty;
        if (ty.dimension.size() + ty.indirect_level > 1) {
          final_ty.indirect_level = 1;
          final_ty.dimension.clear();
          auto offset =
            host_ref.createInstruction(OP_Offset, final_ty, {target});
          for (size_t i = 0; i < ty.indirect_level; ++i)
            offset->args.push_back(host_ref.Zero);
          for (auto &&dim : ty.dimension) {
            auto offset_n = host_ref.createConstantInteger(dim);
            offset->args.push_back(*offset_n);
          }
          target = *offset;
        }
        auto &&init_insn =
          host_ref.createInstruction(OP_Store, final_ty, {target, rv_handle});
      }
    }
  }

  void generateGlobalVariable(GlobalDeclaration *decl) {
    auto ty = convertType(decl->type);
    ty.indirect_level++;
    auto &&g = host->createGlobalVariable();
    g->type = ty;
    g->name = decl->name;
    auto &host_ref = *host;
    host_ref.setInsertPoint(global_initializer_block);
    generateInitializer(decl->initializer, ty, *g);
    scopes.insert(decl->name, *g);
  }

  void globalGeneration() {
    setupGlobalInitializerFunction();
    auto module = root->as<Module *>();
    scopes.enter();
    for (auto decl : module->decls) {
      if (decl->is<GlobalDeclaration *>()) {
        auto p = decl->as_unchecked<GlobalDeclaration *>();
        generateGlobalVariable(p);
      } else if (isFunctionDeclaration(decl)) {
        auto f = decl->as_unchecked<FunctionDeclaration *>();
        functionGeneration(f);
      } else
        throw std::runtime_error("not a supported declaration");
    }
    scopes.exit();
  }

  void generateStatement(NodePtr stmt) {
    auto &host_ref = *host;
    switch (stmt->node_type) {
    case ND_LocalDeclaration: {
      auto decl = stmt->as_unchecked<LocalDeclaration *>();
      auto addr_ty = convertType(decl->type);
      addr_ty.indirect_level++;
      auto var = host_ref.createInstruction(OP_Allocate, addr_ty);
      scopes.insert(decl->name, *var);
      if (decl->initializer)
        generateInitializer(decl->initializer, addr_ty, *var);
      return;
    }
    case ND_CompoundStmt: {
      scopes.enter();
      for (auto &&stmt : stmt->as_unchecked<CompoundStmt *>()->stmts)
        generateStatement(stmt);
      scopes.exit();
      return;
    }
    case ND_IfStmt: {
      auto if_stmt = stmt->as_unchecked<IfStmt *>();
      auto [cond_ty, cond] =
        generateRValue(if_stmt->condition->as_unchecked<Expr *>());
      auto cur_bb = host_ref.getInsertPoint();

      auto then_bb = host_ref.createBasicBlock(*current_function);
      host_ref.setInsertPoint(then_bb);
      generateStatement(if_stmt->then_stmt);

      decltype(cur_bb) else_bb;
      if (if_stmt->else_stmt) {
        else_bb = host_ref.createBasicBlock(*current_function);
        host_ref.setInsertPoint(else_bb);
        generateStatement(if_stmt->else_stmt);
      }

      auto cont_bb = host_ref.createBasicBlock(*current_function);

      host_ref.setInsertPoint(cur_bb);
      host_ref.createInstruction(
        OP_Br, cond_ty,
        {cond, *then_bb, if_stmt->else_stmt ? *else_bb : *cont_bb});

      host_ref.setInsertPoint(then_bb);
      host_ref.createInstruction(OP_Jump, VoidType, {*cont_bb});

      if (if_stmt->else_stmt) {
        host_ref.setInsertPoint(else_bb);
        host_ref.createInstruction(OP_Jump, VoidType, {*cont_bb});
      }

      host_ref.setInsertPoint(cont_bb);
      current_function->basic_block.push_back(*then_bb);
      if (if_stmt->else_stmt)
        current_function->basic_block.push_back(*else_bb);
      current_function->basic_block.push_back(*cont_bb);
      return;
    }
    case ND_WhileStmt: {
      auto while_stmt = stmt->as_unchecked<WhileStmt *>();
      auto cur_bb = host_ref.getInsertPoint();

      auto end_bb = host_ref.createBasicBlock(*current_function);

      auto cond_bb = host_ref.createBasicBlock(*current_function);
      host_ref.setInsertPoint(cond_bb);
      auto [cond_ty, cond] =
        generateRValue(while_stmt->condition->as_unchecked<Expr *>());

      bb_break.push_back(end_bb);
      bb_continue.push_back(cond_bb);

      auto body_bb = host_ref.createBasicBlock(*current_function);
      host_ref.setInsertPoint(body_bb);
      generateStatement(while_stmt->body);

      bb_break.pop_back();
      bb_continue.pop_back();

      host_ref.setInsertPoint(cur_bb);
      host_ref.createInstruction(OP_Jump, VoidType, {*cond_bb});

      host_ref.setInsertPoint(cond_bb);
      host_ref.createInstruction(OP_Br, cond_ty, {cond, *body_bb, *end_bb});

      host_ref.setInsertPoint(end_bb);

      for (auto &&bb : {cond_bb, body_bb, end_bb})
        current_function->basic_block.push_back(*bb);
      return;
    }
    case ND_ContinueStmt: {
      if (bb_continue.empty())
        throw std::runtime_error("continue statement outside of loop");
      host_ref.createInstruction(OP_Jump, VoidType, {*bb_continue.back()});
      return;
    }
    case ND_BreakStmt: {
      if (bb_break.empty())
        throw std::runtime_error("break statement outside of loop");
      host_ref.createInstruction(OP_Jump, VoidType, {*bb_break.back()});
      return;
    }
    case ND_ReturnStmt: {
      auto ret_stmt = stmt->as_unchecked<ReturnStmt *>();
      if (ret_stmt->value) {
        auto [ret_ty, ret_handle] = generateRValue(ret_stmt->value);
        host_ref.createInstruction(OP_Store, current_retval->type,
                                   {*current_retval, ret_handle});
      }
      host_ref.createInstruction(OP_Jump, VoidType, {*current_return_bb});
      return;
    }
    default:
      generateRValue(stmt->as_unchecked<Expr *>());
    }
  }

  void functionGeneration(FunctionDeclaration *decl) {
    auto &host_ref = *host;
    auto f = host_ref.createFunction();
    f->return_type = convertType(decl->return_type);
    f->name = decl->name;
    current_function = f;

    scopes.insert(decl->name, *f);

    bool is_external = decl->body == nullptr;
    f->external = is_external;
    bool is_void =
      f->return_type.primitive_type == SSAType::PrimitiveType::Void;

    BasicBlock *return_bb = nullptr;
    if (!is_external) {
      auto entry_bb = host_ref.createBasicBlock(*f);
      f->basic_block.push_back(*entry_bb);
      host_ref.setInsertPoint(entry_bb);

      if (!is_void) {
        auto ty = f->return_type;
        ty.indirect_level++;
        current_retval = host_ref.createInstruction(OP_Allocate, ty);
      } else {
        current_retval = nullptr;
      }

      return_bb = host_ref.createBasicBlock(*f);
      host_ref.setInsertPoint(return_bb);
      current_return_bb = return_bb;

      if (!is_void) {
        auto retval = host_ref.createInstruction(OP_Load, f->return_type,
                                                 {*current_retval});
        host_ref.createInstruction(OP_Return, f->return_type, {*retval});
      } else {
        host_ref.createInstruction(OP_Return, VoidType);
      }

      host_ref.setInsertPoint(entry_bb);
    }

    scopes.enter();
    for (auto &&param : decl->parameters) {
      auto arg = host_ref.createArgument(*f);
      arg->name = param.first;
      arg->type = convertParameterType(param.second);
      f->args.push_back(*arg);

      if (!is_external) {
        auto addr_ty = arg->type;
        addr_ty.indirect_level += 1;
        auto arg_addr = host_ref.createInstruction(OP_Allocate, addr_ty);

        host_ref.createInstruction(OP_Store, addr_ty, {*arg_addr, *arg});
        scopes.insert(arg->name, *arg_addr);
      }
    }
    if (!is_external) {
      scopes.enter();
      generateStatement(decl->body);
      scopes.exit();
      f->basic_block.push_back(*return_bb);
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