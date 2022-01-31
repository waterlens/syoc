#pragma once

#include "IR/IR.hpp"
#include "Tree/Tree.hpp"
#include "Util/TrivialValueVector.hpp"

#include <algorithm>
#include <cassert>
#include <cstddef>
#include <functional>
#include <numeric>
#include <stdexcept>
#include <string_view>
#include <tuple>
#include <vector>

class Tree2SSA {
  using TypeHandle = std::pair<SSAType, const SSAValue &>;
  NodePtr root;
  IRHost *host;
  BasicBlock *global_initializer_block, *current_return_bb, *current_alloca_bb,
    *current_entry_bb;
  Function *current_function;
  Instruction *current_retval;
  Scope<SSAValueHandle> scopes;
  std::vector<BasicBlock *> bb_break, bb_continue;

  void setupGlobalInitializerFunction() {
    auto init = host->createFunction();
    auto bb = host->createBasicBlock(*init);

    init->name = "__syoc@init";
    init->args = {};
    init->return_type = VoidType;
    init->basic_block.push_back(*bb);
    init->external = false;

    global_initializer_block = bb;
  }

  SSAType convertType(const Type &ty) {
    SSAType ssa_ty;
    if (ty.spec == TS_Int)
      ssa_ty = IntType;
    else if (ty.spec == TS_Void)
      ssa_ty = VoidType;
    for (auto &&dim : ty.dim)
      ssa_ty.dim.push_back(dim->as<IntegerLiteral *>()->value);
    return ssa_ty;
  }

  size_t calculateArrayTotalLength(SSAType ty) {
    return std::accumulate(
      ty.dim.begin(), ty.dim.end(),
      ty.width / CHAR_BIT *
        (ty.primitive_type == SSAType::PrimitiveType::Void ? 0 : 1),
      [](auto lhs, auto rhs) { return lhs * rhs; });
  }

  void clearExtraJump(BasicBlock *bb) {
    for (auto p = bb->insn.begin(); p != bb->insn.end(); ++p) {
      auto v = *p;
      if (auto insn = (*host)[v].as<Instruction *>()) {
        if (insn->op == OP_Jump || insn->op == OP_Br) {
          bb->insn.resize(p - bb->insn.begin() + 1);
          break;
        }
      } else
        throw std::runtime_error("not a instruction");
    }
  }

  void addBasicBlockToCurrentFunction(BasicBlock *bb) {
    current_function->basic_block.push_back(*bb);
  }

  TypeHandle findInScope(std::string_view name) {
    auto handle = scopes.find(name);
    if (handle == 0 || !handle.isValid())
      throw std::runtime_error(fmt::format("undefined variable: {}", name));
    auto &value = (*host)[handle];
    SSAType ty;
    if (value.is<Instruction *>() &&
        value.as_unchecked<Instruction *>()->op == OP_Allocate) {
      ty = value.as_unchecked<Instruction *>()->type;
    } else if (value.is<GlobalVariable *>()) {
      ty = value.as_unchecked<GlobalVariable *>()->type;
    } else
      throw std::runtime_error("name is not variable");
    return TypeHandle{ty, value};
  }

  TypeHandle generateLValue(ExprPtr expr) {
    if (auto arr_sub = expr->as<ArraySubscriptExpr *>()) {
      auto [arr_ty, arr_handle] = generateLValue(arr_sub->array);
      auto [idx_ty, idx_handle] = generateRValue(arr_sub->subscript);
      auto new_ty = arr_ty;
      auto n = new_ty.dim.front();
      new_ty.dim.pop_front();
      auto elem = host->createInstruction(
        OP_Offset, new_ty,
        {arr_handle, *host->createConstantInteger(n), idx_handle});
      return {new_ty, *elem};
    } else if (auto ref = expr->as<RefExpr *>()) {
      auto th = findInScope(ref->name);
      return th;
    } else if (auto assign = expr->as<AssignExpr *>()) {
      auto [lhs_ty, lhs_handle] = generateLValue(assign->lhs);
      auto [rhs_ty, rhs_handle] = generateRValue(assign->rhs);

      auto assign_insn =
        host->createInstruction(OP_Store, VoidType, {lhs_handle, rhs_handle});

      return {lhs_ty, lhs_handle};
    } else
      throw std::runtime_error("not a supported expression");
  }

  TypeHandle generateShortCircuit(BinaryExpr *binary) {
    auto [lhs_ty, lhs] = generateRValue(binary->lhs);
    auto tmp_addr_ty = lhs_ty;
    tmp_addr_ty.pointer++;

    auto tmp_var_addr = host->createInstruction(
      OP_Allocate, tmp_addr_ty,
      {*host->createConstantInteger(calculateArrayTotalLength(tmp_addr_ty))},
      current_alloca_bb);
    host->createInstruction(OP_Store, VoidType, {*tmp_var_addr, lhs});

    auto rhs_eval_bb = host->createBasicBlock(*current_function);
    auto next_bb = host->createBasicBlock(*current_function);
    auto branch = host->createInstruction(OP_Br, IntType, {lhs});

    if (binary->op == OP_Land) {
      branch->args.push_back(*rhs_eval_bb);
      branch->args.push_back(*next_bb);
    } else {
      branch->args.push_back(*next_bb);
      branch->args.push_back(*rhs_eval_bb);
    }

    host->setInsertPoint(rhs_eval_bb);
    auto [rhs_ty, rhs] = generateRValue(binary->rhs);
    host->createInstruction(OP_Store, VoidType, {*tmp_var_addr, rhs});
    host->createInstruction(OP_Jump, IntType, {*next_bb});

    host->setInsertPoint(next_bb);
    auto final = host->createInstruction(OP_Load, lhs_ty, {*tmp_var_addr});

    addBasicBlockToCurrentFunction(rhs_eval_bb);
    addBasicBlockToCurrentFunction(next_bb);

    return {lhs_ty, *final};
  }

  TypeHandle generateRValue(ExprPtr expr) {
    switch (expr->node_type) {
    case ND_IntegerLiteral: {
      auto lit = host->createConstantInteger(
        expr->as_unchecked<IntegerLiteral *>()->value);
      return {IntType, *lit};
    }
    case ND_UnaryExpr: {
      auto unary = expr->as_unchecked<UnaryExpr *>();
      auto [operand_ty, operand_handle] = generateRValue(unary->operand);
      if (unary->op == OP_Neg) {
        return {operand_ty,
                *host->createInstruction(OP_Neg, operand_ty, {operand_handle})};
      }
      if (unary->op == OP_Lnot) {
        return {IntType,
                *host->createInstruction(OP_Lnot, IntType, {operand_handle})};
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
        assert(lhs_ty.dim.empty() && rhs_ty.dim.empty());
        return {lhs_ty,
                *host->createInstruction(binary->op, lhs_ty, {lhs, rhs})};
      }
    }
    case ND_CallExpr: {
      auto call = expr->as_unchecked<CallExpr *>();
      auto name = call->func->as_unchecked<RefExpr *>()->name;
      auto handle = scopes.find(name);
      if (!handle.isValid() || handle.id == 0)
        throw std::runtime_error("can't found function");
      auto f = (*host)[handle].as<Function *>();
      TrivialValueVector<SSAValueHandle, 3> args;
      for (auto &&arg : call->args) {
        auto [arg_ty, arg_handle] = generateRValue(arg);
        args.push_back(arg_handle);
      }
      auto call_insn =
        host->createInstruction(OP_Call, f->return_type, {handle});
      for (auto &&arg : args) call_insn->args.push_back(arg);
      return {f->return_type, *call_insn};
    }
    case ND_ArraySubscriptExpr:
    case ND_RefExpr:
    case ND_AssignExpr: {
      auto [lv_ty, lv_handle] = generateLValue(expr);
      return generateLoad(lv_ty, lv_handle);
    }
    default:
      throw std::runtime_error("not a supported expression");
    }
  }

  void arrayZeroInitializer(TypeHandle th) {
    // targer should be the array head element pointer
    auto [ty, target] = th;
    assert(ty.dim.size());
    auto len = calculateArrayTotalLength(ty);
    auto len_constant = host->createConstantInteger(len);

    auto set_insn = host->createInstruction(OP_Memset0, VoidType);
    set_insn->args = {target, *len_constant};
  }

  void generateStore(ExprPtr expr, SSAValueHandle target) {
    if (!expr)
      return;
    auto [rv_ty, rv_handle] = generateRValue(expr);
    host->createInstruction(OP_Store, VoidType, {target, rv_handle});
  }

  TypeHandle generateLoad(const SSAType &ty, SSAValueHandle target) {
    return {
      ty.createDereference(),
      *host->createInstruction(OP_Load, ty.createDereference(), {target})};
  }

  void generateListInitializer(InitListExpr *init, TypeHandle th) {
    auto ty = th.first;
    auto arr = th.second;
    std::vector<std::pair<ExprPtr, unsigned>> stack;
    std::vector<unsigned> array_idx(ty.dim.size(), 0);
    std::vector<unsigned> current_limit;
    unsigned idx = 0;
    enum { BRACE_END };

    auto adjust_array_index = [&]() {
      if (array_idx.size())
        array_idx.back()++;
      for (int i = (int)(array_idx.size() - 1); i >= 0; --i) {
        if (array_idx[i] >= ty.dim[i]) {
          array_idx[i] = 0;
          if (i - 1 >= 0)
            array_idx[i - 1]++;
        }
      };
    };

    stack.emplace_back(init, 0);
    while (!stack.empty()) {
      auto [expr, level] = stack.back();
      stack.pop_back();
      if (!expr) {
        if (level == BRACE_END) {
          idx = 0;
          current_limit.pop_back();
          auto [_1, level2] = stack.back();
          stack.pop_back();
          auto [_2, last_index] = stack.back();
          stack.pop_back();
          if (level2 >= 1)
            if (array_idx[level2 - 1] == last_index)
              array_idx[level2 - 1]++; // brace end, trailing elements must be
                                       // all zero, skip to next index
          if (array_idx.begin() + level2 <= array_idx.end())
            std::fill(array_idx.begin() + level2, array_idx.end(), 0);
        }
      } else if (auto init = expr->as<InitListExpr *>()) {
        stack.emplace_back(nullptr, level >= 1 ? array_idx[level - 1] : 0);
        stack.emplace_back(nullptr, level);
        stack.emplace_back(nullptr, BRACE_END);
        for (auto iter = init->values.rbegin(); iter != init->values.rend();
             ++iter)
          stack.emplace_back(*iter, level + 1);
        current_limit.emplace_back(
          std::accumulate(ty.dim.begin() + level, ty.dim.end(), 1,
                          std::multiplies<unsigned>()));
        idx = 0;
      } else {
        if (level > ty.dim.size())
          throw std::runtime_error("initializer list too deep");
        if (idx >= current_limit.back())
          throw std::runtime_error("too many initializer elements");
        auto offset =
          host->createInstruction(OP_Offset, IntType.createReference(), {arr});
        for (size_t i = 0; i < array_idx.size(); ++i) {
          offset->args.push_back(*host->createConstantInteger(ty.dim[i]));
          offset->args.push_back(*host->createConstantInteger(array_idx[i]));
        }
        generateStore(expr, *offset);
        adjust_array_index();
        idx++;
      }
    }
  }

  void generateInitializer(ExprPtr init, TypeHandle th) {
    // target should be a pointer
    // it is either an address to local/global scalar variable
    // or a pointer to the array head element address

    auto ty = th.first;
    auto target = th.second;

    if (init) {
      if (auto l = init->as<InitListExpr *>())
        generateListInitializer(l, {ty, target});
      else
        generateStore(init, target);
    }
  }

  void generateGlobalVariable(GlobalDeclaration *decl) {
    auto ty = convertType(decl->type);
    ty.reference();
    auto g = host->createGlobalVariable();
    g->type = ty;
    g->name = decl->name;
    host->setInsertPoint(global_initializer_block);
    generateInitializer(decl->initializer, {ty, *g});
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
    switch (stmt->node_type) {
    case ND_LocalDeclaration: {
      auto decl = stmt->as_unchecked<LocalDeclaration *>();
      auto addr_ty = convertType(decl->type);
      addr_ty.reference();
      auto var = host->createInstruction(
        OP_Allocate, addr_ty,
        {*host->createConstantInteger(calculateArrayTotalLength(addr_ty))},
        current_alloca_bb);
      scopes.insert(decl->name, *var);
      if (decl->initializer)
        generateInitializer(decl->initializer, findInScope(decl->name));
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
      auto cur_bb = host->getInsertPoint();
      auto then_bb = host->createBasicBlock(*current_function);
      auto cont_bb = host->createBasicBlock(*current_function);
      decltype(cur_bb) else_bb;

      addBasicBlockToCurrentFunction(then_bb);
      host->setInsertPoint(then_bb);
      generateStatement(if_stmt->then_stmt);
      host->createInstruction(OP_Jump, VoidType, {*cont_bb});

      if (if_stmt->else_stmt) {
        else_bb = host->createBasicBlock(*current_function);
        addBasicBlockToCurrentFunction(else_bb);
        host->setInsertPoint(else_bb);
        generateStatement(if_stmt->else_stmt);
        host->createInstruction(OP_Jump, VoidType, {*cont_bb});
      }

      host->setInsertPoint(cur_bb);
      host->createInstruction(
        OP_Br, cond_ty,
        {cond, *then_bb, if_stmt->else_stmt ? *else_bb : *cont_bb});

      addBasicBlockToCurrentFunction(cont_bb);
      host->setInsertPoint(cont_bb);
      return;
    }
    case ND_WhileStmt: {
      auto while_stmt = stmt->as_unchecked<WhileStmt *>();
      auto cur_bb = host->getInsertPoint();
      auto end_bb = host->createBasicBlock(*current_function);
      auto cond_bb = host->createBasicBlock(*current_function);

      host->setInsertPoint(cond_bb);
      addBasicBlockToCurrentFunction(cond_bb);
      auto [cond_ty, cond] =
        generateRValue(while_stmt->condition->as_unchecked<Expr *>());

      bb_break.push_back(end_bb);
      bb_continue.push_back(cond_bb);

      auto body_bb = host->createBasicBlock(*current_function);
      addBasicBlockToCurrentFunction(body_bb);
      host->setInsertPoint(body_bb);
      generateStatement(while_stmt->body);
      host->createInstruction(OP_Jump, VoidType, {*cond_bb});

      addBasicBlockToCurrentFunction(end_bb);

      bb_break.pop_back();
      bb_continue.pop_back();

      host->setInsertPoint(cur_bb);
      host->createInstruction(OP_Jump, VoidType, {*cond_bb});

      host->setInsertPoint(cond_bb);
      host->createInstruction(OP_Br, cond_ty, {cond, *body_bb, *end_bb});

      host->setInsertPoint(end_bb);

      return;
    }
    case ND_ContinueStmt: {
      if (bb_continue.empty())
        throw std::runtime_error("continue statement outside of loop");
      host->createInstruction(OP_Jump, VoidType, {*bb_continue.back()});
      return;
    }
    case ND_BreakStmt: {
      if (bb_break.empty())
        throw std::runtime_error("break statement outside of loop");
      host->createInstruction(OP_Jump, VoidType, {*bb_break.back()});
      return;
    }
    case ND_ReturnStmt: {
      auto ret_stmt = stmt->as_unchecked<ReturnStmt *>();
      if (ret_stmt->value) {
        auto [ret_ty, ret_handle] = generateRValue(ret_stmt->value);
        host->createInstruction(OP_Store, VoidType,
                                {*current_retval, ret_handle});
      }
      host->createInstruction(OP_Jump, VoidType, {*current_return_bb});
      return;
    }
    default:
      generateRValue(stmt->as_unchecked<Expr *>());
    }
  }

  void functionGeneration(FunctionDeclaration *decl) {
    auto f = host->createFunction();
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
      auto alloca_bb = host->createBasicBlock(*f);
      f->basic_block.push_back(*alloca_bb);
      current_alloca_bb = alloca_bb;
      host->setInsertPoint(alloca_bb);

      auto entry_bb = host->createBasicBlock(*f);
      f->basic_block.push_back(*entry_bb);
      current_entry_bb = entry_bb;
      host->setInsertPoint(entry_bb);

      if (!is_void) {
        auto ty = f->return_type;
        ty.reference();
        current_retval = host->createInstruction(
          OP_Allocate, ty,
          {*host->createConstantInteger(calculateArrayTotalLength(ty))},
          current_alloca_bb);
      } else {
        current_retval = nullptr;
      }

      return_bb = host->createBasicBlock(*f);
      host->setInsertPoint(return_bb);
      current_return_bb = return_bb;

      if (!is_void) {
        auto retval =
          host->createInstruction(OP_Load, f->return_type, {*current_retval});
        host->createInstruction(OP_Return, f->return_type, {*retval});
      } else {
        host->createInstruction(OP_Return, VoidType);
      }

      host->setInsertPoint(entry_bb);
    }

    scopes.enter();
    for (auto &&param : decl->parameters) {
      auto arg = host->createArgument(*f);
      arg->name = param.first;
      arg->type = convertType(param.second);
      f->args.push_back(*arg);

      if (!is_external) {
        auto addr_ty = arg->type;
        addr_ty.reference();
        auto arg_addr = host->createInstruction(
          OP_Allocate, addr_ty,
          {*host->createConstantInteger(calculateArrayTotalLength(addr_ty))},
          current_alloca_bb);
        host->createInstruction(OP_Store, VoidType, {*arg_addr, *arg});
        scopes.insert(arg->name, *arg_addr);
      }
    }
    if (!is_external) {
      scopes.enter();
      generateStatement(decl->body);
      scopes.exit();
      f->basic_block.push_back(*return_bb);
      host->createInstruction(OP_Jump, VoidType, {*current_entry_bb},
                              current_alloca_bb);
      for (auto &&b : f->basic_block)
        clearExtraJump((*host)[b].as<BasicBlock *>());
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