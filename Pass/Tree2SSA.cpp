#include "Tree2SSA.hpp"
#include "IR/IR.hpp"
#include <stdexcept>

namespace SyOC {
void Tree2SSA::setupGlobalInitializerFunction() {
  auto *init =
    Function::create(PredefinedType::Void, "__syoc_init", host->getModule());
  auto *bb = BasicBlock::create(init);

  init->refExternal() = false;
  global_initializer_block = bb;
}

Tree2SSA::TypeDimensionValue Tree2SSA::findInScope(std::string_view name) {
  auto r = scopes.find(name);
  if (r.second == nullptr)
    throw std::runtime_error(fmt::format("undefined variable: {}", name));
  return r;
}

Tree2SSA::TypeDimensionValue Tree2SSA::generateLValue(ExprPtr expr,
                                                      bool raise_exception) {
  if (auto *arr_sub = expr->as<TreeArraySubscriptExpr *>()) {
    auto [arr_ty, arr] = generateLValue(arr_sub->array);
    auto [idx_ty, idx] = generateRValue(arr_sub->subscript);
    if (arr_ty.second.empty())
      throw std::runtime_error("can't get the dimension of array type");
    auto bound = arr_ty.second.front();
    arr_ty.second.pop_front();
    auto offset_unit_size = calculateArrayTotalLength(arr_ty);
    auto *elem =
      host->createInstruction(OP_Offset, PredefinedType::IntPtr,
                              {arr, ConstantInteger::create(bound), idx,
                               ConstantInteger::create(offset_unit_size)});
    return {{PredefinedType::IntPtr, arr_ty.second}, elem};
  }
  if (auto *ref = expr->as<TreeRefExpr *>()) {
    return findInScope(ref->name);
  }
  if (auto *assign = expr->as<TreeAssignExpr *>()) {
    auto [lhs_ty, lhs] = generateLValue(assign->lhs);
    auto [rhs_ty, rhs] = generateRValue(assign->rhs);

    host->createInstruction(OP_Store, PredefinedType::Void, {lhs, rhs});
    return {lhs_ty, lhs};
  }
  if (raise_exception)
    throw std::runtime_error("not a supported expression");
  return {{PredefinedType::Void, {}}, nullptr};
}

Tree2SSA::TypeDimensionValue
Tree2SSA::generateShortCircuit(TreeBinaryExpr *binary) {
  auto [lhs_ty, lhs] = generateRValue(binary->lhs);
  auto ty = lhs_ty.first;
  ty.pointer++;

  auto *tmp_var = host->createInstruction(
    OP_Allocate, ty,
    {ConstantInteger::create(calculateArrayTotalLength(lhs_ty))},
    current_alloca_bb);

  host->createInstruction(OP_Store, PredefinedType::Void, {tmp_var, lhs});

  auto *cur_bb = host->getInsertPoint();
  auto *rhs_eval_bb = BasicBlock::create(current_function);
  auto *next_bb = BasicBlock::create(current_function);

  if (binary->op == OP_Land)
    cur_bb->linkByBranch(lhs, rhs_eval_bb, next_bb);
  else if (binary->op == OP_Lor)
    cur_bb->linkByBranch(lhs, next_bb, rhs_eval_bb);

  host->setInsertPoint(rhs_eval_bb);
  auto [rhs_ty, rhs] = generateRValue(binary->rhs);
  host->createInstruction(OP_Store, PredefinedType::Void, {tmp_var, rhs});
  host->getInsertPoint()->linkByJump(next_bb);

  host->setInsertPoint(next_bb);
  auto *final = host->createInstruction(OP_Load, lhs_ty.first, {tmp_var});
  return {lhs_ty, final};
}

Tree2SSA::TypeDimensionValue Tree2SSA::generateRValue(ExprPtr expr) {
  switch (expr->node_type) {
  case ND_IntegerLiteral: {
    auto *lit = ConstantInteger::create(
      expr->as_unchecked<TreeIntegerLiteral *>()->value);
    return {{PredefinedType::Int32, {}}, lit};
  }
  case ND_UnaryExpr: {
    auto *unary = expr->as_unchecked<TreeUnaryExpr *>();
    auto [operand_ty, operand] = generateRValue(unary->operand);
    if (unary->op == OP_Neg) {
      return {operand_ty,
              host->createInstruction(OP_Neg, operand_ty.first, {operand})};
    }
    if (unary->op == OP_Lnot) {
      return {
        {PredefinedType::Int32, {}},
        host->createInstruction(OP_Lnot, PredefinedType::Int32, {operand})};
    }
    throw std::runtime_error("not a supported unary operator");
  }
  case ND_BinaryExpr: {
    auto *binary = expr->as_unchecked<TreeBinaryExpr *>();
    if (binary->op == OP_Land || binary->op == OP_Lor) {
      return generateShortCircuit(binary);
    }
    auto [lhs_ty, lhs] = generateRValue(binary->lhs);
    auto [rhs_ty, rhs] = generateRValue(binary->rhs);
    assert(lhs_ty.second.empty() && rhs_ty.second.empty());
    return {lhs_ty,
            host->createInstruction(binary->op, lhs_ty.first, {lhs, rhs})};
  }
  case ND_CallExpr: {
    auto *call = expr->as_unchecked<TreeCallExpr *>();
    auto name = call->func->as_unchecked<TreeRefExpr *>()->name;
    auto func = scopes.find(name);
    if (func.second == nullptr)
      throw std::runtime_error("can't found function");
    auto *f = func.second->as<Function *>();
    TrivialValueVector<Value *, 3> args;
    for (auto *arg : call->args) {
      auto [arg_ty, arg_gen] = generateArgumentValue(arg);
      args.push_back(arg_gen);
    }
    auto *call_insn = host->createInstruction(OP_Call, f->return_type, {f});
    for (auto *arg : args) call_insn->addInput(arg);
    return {{f->return_type, {}}, call_insn};
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

Tree2SSA::TypeDimensionValue Tree2SSA::generateArgumentValue(ExprPtr expr) {
  // if it's an array, we don't load it
  auto [ty, val] = generateLValue(expr);
  if (val == nullptr)
    return generateRValue(expr);
  if (ty.second.empty()) {
    return generateLoad(ty, val);
  }
  return {ty, val};
}
void Tree2SSA::arrayZeroInitializer(const Tree2SSA::TypeDimensionValue &th) {
  // target should be the array head element pointer
  auto [ty, target] = th;
  assert(!ty.second.empty());
  auto len = calculateArrayTotalLength(ty);
  auto *len_constant = ConstantInteger::create(len);

  host->createInstruction(OP_Memset0, PredefinedType::Void,
                          {target, len_constant});
}
void Tree2SSA::generateStore(ExprPtr expr, Value *target) {
  if (expr == nullptr)
    return;
  auto [rv_ty, rv_handle] = generateRValue(expr);
  host->createInstruction(OP_Store, PredefinedType::Void, {target, rv_handle});
}

Tree2SSA::TypeDimensionValue
Tree2SSA::generateLoad(const Tree2SSA::TypeDimension &td, Value *target) {
  return {
    {td.first.createDereference(), td.second},
    host->createInstruction(OP_Load, td.first.createDereference(), {target})};
}

void Tree2SSA::generateListInitializer(TreeInitListExpr *init,
                                       const Tree2SSA::TypeDimensionValue &th) {
  const auto &ty = th.first.first;
  const auto &dim = th.first.second;
  const auto &arr = th.second;
  std::vector<std::pair<ExprPtr, unsigned>> stack;
  std::vector<unsigned> array_idx(dim.size(), 0);
  std::vector<unsigned> current_limit;
  unsigned idx = 0;
  enum { BRACE_END };

  auto adjust_array_index = [&]() {
    if (!array_idx.empty())
      array_idx.back()++;
    for (int i = (int)(array_idx.size() - 1); i >= 0; --i) {
      if (array_idx[i] >= dim[i]) {
        array_idx[i] = 0;
        if (i - 1 >= 0)
          array_idx[i - 1]++;
      }
    }
  };

  stack.emplace_back(init, 0);
  while (!stack.empty()) {
    auto [expr, level] = stack.back();
    stack.pop_back();
    if (expr == nullptr) {
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
    } else if (auto *init = expr->as<TreeInitListExpr *>()) {
      stack.emplace_back(nullptr, level >= 1 ? array_idx[level - 1] : 0);
      stack.emplace_back(nullptr, level);
      stack.emplace_back(nullptr, BRACE_END);
      for (auto iter = init->values.rbegin(); // NOLINT
           iter != init->values.rend(); ++iter)
        stack.emplace_back(*iter, level + 1);
      current_limit.emplace_back(std::accumulate(dim.begin() + level, dim.end(),
                                                 1, std::multiplies<>()));
      idx = 0;
    } else {
      if (level > dim.size())
        throw std::runtime_error("initializer list too deep");
      if (idx >= current_limit.back())
        throw std::runtime_error("too many initializer elements");
      auto *offset =
        host->createInstruction(OP_Offset, PredefinedType::IntPtr, {arr});
      for (size_t i = 0; i < array_idx.size(); ++i) {
        offset->addInput(ConstantInteger::create(dim[i]));
        offset->addInput(ConstantInteger::create(array_idx[i]));
      }
      offset->addInput(
        ConstantInteger::create(calculateArrayBaseUnitSize(th.first)));
      generateStore(expr, offset);
      adjust_array_index();
      idx++;
    }
  }
}

void Tree2SSA::generateInitializer(ExprPtr init,
                                   const Tree2SSA::TypeDimensionValue &th) {
  auto ty = th.first;
  auto *target = th.second;

  if (init != nullptr) {
    if (auto *l = init->as<TreeInitListExpr *>())
      generateListInitializer(l, {ty, target});
    else
      generateStore(init, target);
  }
}

void Tree2SSA::generateGlobalVariable(TreeGlobalDeclaration *decl) {
  auto ty = convertType(decl->type);
  ty.first.reference();
  auto *g =
    GlobalVariable::create(ty.first, decl->name, calculateArrayTotalLength(ty));
  host->getModule()->global.push_back(g);
  host->setInsertPoint(global_initializer_block);
  generateInitializer(decl->initializer, {ty, g});
  scopes.insert(decl->name, {ty, g});
}

void Tree2SSA::globalGeneration() {
  setupGlobalInitializerFunction();
  auto *module = root->as<TreeModule *>();
  scopes.enter();
  for (auto *decl : module->decls) {
    if (decl->is<TreeGlobalDeclaration *>()) {
      auto *p = decl->as_unchecked<TreeGlobalDeclaration *>();
      generateGlobalVariable(p);
    } else if (isFunctionDeclaration(decl)) {
      auto *f = decl->as_unchecked<TreeFunctionDeclaration *>();
      functionGeneration(f);
    } else
      throw std::runtime_error("not a supported declaration");
  }
  host->createInstruction(OP_Return, PredefinedType::Void, {},
                          global_initializer_block);
  scopes.exit();
}

void Tree2SSA::generateStatement(NodePtr stmt) {
  switch (stmt->node_type) {
  case ND_LocalDeclaration: {
    auto *decl = stmt->as_unchecked<TreeLocalDeclaration *>();
    auto addr_ty = convertType(decl->type);
    // Pointer nesting levels counting.
    addr_ty.first.reference();
    auto *var = host->createInstruction(
      OP_Allocate, addr_ty.first,
      {ConstantInteger::create(calculateArrayTotalLength(addr_ty))},
      current_alloca_bb);
    scopes.insert(decl->name, {addr_ty, var});
    if (decl->initializer != nullptr)
      generateInitializer(decl->initializer, {addr_ty, var});
    return;
  }
  case ND_CompoundStmt: {
    scopes.enter();
    for (auto &&stmt : stmt->as_unchecked<TreeCompoundStmt *>()->stmts)
      generateStatement(stmt);
    scopes.exit();
    return;
  }
  case ND_IfStmt: {
    auto *if_stmt = stmt->as_unchecked<TreeIfStmt *>();
    auto [cond_ty, cond] =
      generateRValue(if_stmt->condition->as_unchecked<TreeExpr *>());
    auto *cur_bb = host->getInsertPoint();
    auto *then_bb = BasicBlock::create(current_function);
    auto *cont_bb = BasicBlock::create();
    auto *else_bb = BasicBlock::create();

    host->setInsertPoint(then_bb);
    generateStatement(if_stmt->then_stmt);
    host->getInsertPoint()->linkByJump(cont_bb);

    if (if_stmt->else_stmt != nullptr) {
      current_function->addBasicBlock(else_bb);
      host->setInsertPoint(else_bb);
      generateStatement(if_stmt->else_stmt);
      host->getInsertPoint()->linkByJump(cont_bb);
    } else {
      delete else_bb;
      else_bb = nullptr;
    }

    cur_bb->linkByBranch(cond, then_bb,
                         if_stmt->else_stmt != nullptr ? else_bb : cont_bb);

    current_function->addBasicBlock(cont_bb);
    host->setInsertPoint(cont_bb);
    return;
  }
  case ND_WhileStmt: {
    auto *while_stmt = stmt->as_unchecked<TreeWhileStmt *>();
    auto *cur_bb = host->getInsertPoint();
    auto *cond_bb_begin = BasicBlock::create(current_function);
    auto *end_bb = BasicBlock::create();

    host->setInsertPoint(cond_bb_begin);
    auto [cond_ty, cond] =
      generateRValue(while_stmt->condition->as_unchecked<TreeExpr *>());
    auto *cond_bb_end = host->getInsertPoint();

    bb_break.push_back(end_bb);
    bb_continue.push_back(cond_bb_begin);

    auto *body_bb = BasicBlock::create(current_function);
    host->setInsertPoint(body_bb);
    generateStatement(while_stmt->body);
    host->getInsertPoint()->linkByJump(cond_bb_begin);

    current_function->addBasicBlock(end_bb);

    bb_break.pop_back();
    bb_continue.pop_back();

    cur_bb->linkByJump(cond_bb_begin);
    cond_bb_end->linkByBranch(cond, body_bb, end_bb);
    host->setInsertPoint(end_bb);

    return;
  }
  case ND_ContinueStmt: {
    if (bb_continue.empty())
      throw std::runtime_error("continue statement outside of loop");
    host->getInsertPoint()->linkByJump(bb_continue.back());
    return;
  }
  case ND_BreakStmt: {
    if (bb_break.empty())
      throw std::runtime_error("break statement outside of loop");
    host->getInsertPoint()->linkByJump(bb_break.back());
    return;
  }
  case ND_ReturnStmt: {
    auto *ret_stmt = stmt->as_unchecked<TreeReturnStmt *>();
    if (ret_stmt->value != nullptr) {
      auto [ret_ty, ret] = generateRValue(ret_stmt->value);
      host->createInstruction(OP_Store, PredefinedType::Void,
                              {current_retval, ret});
    }
    host->getInsertPoint()->linkByJump(current_return_bb);
    return;
  }
  default:
    generateRValue(stmt->as_unchecked<TreeExpr *>());
  }
}

void Tree2SSA::functionGeneration(TreeFunctionDeclaration *decl) {
  auto ty = convertType(decl->return_type);
  current_function = Function::create(ty.first, decl->name, host->getModule());
  scopes.insert(decl->name, {ty, current_function});

  current_function->refExternal() = decl->body == nullptr;
  bool is_external = current_function->refExternal();
  bool is_void = ty.first.primitive_type == Type::PrimitiveType::Void;

  BasicBlock *return_bb = nullptr;
  if (!is_external) {
    auto *alloca_bb = BasicBlock::create(current_function);
    current_alloca_bb = alloca_bb;

    auto *entry_bb = BasicBlock::create(current_function);
    current_entry_bb = entry_bb;
    host->setInsertPoint(entry_bb);

    if (!is_void) {
      auto ty = current_function->return_type;
      ty.reference();
      current_retval = host->createInstruction(
        OP_Allocate, ty,
        {ConstantInteger::create(calculateArrayTotalLength({ty, {}}))},
        current_alloca_bb);
    } else {
      current_retval = nullptr;
    }

    return_bb = BasicBlock::create();
    host->setInsertPoint(return_bb);
    current_return_bb = return_bb;

    if (!is_void) {
      auto *retval = host->createInstruction(
        OP_Load, current_function->return_type, {current_retval});
      host->createInstruction(OP_Return, PredefinedType::Void, {retval});
    } else {
      host->createInstruction(OP_Return, PredefinedType::Void);
    }

    host->setInsertPoint(entry_bb);
  }

  scopes.enter();
  for (auto &&param : decl->parameters) {
    auto ty = convertType(param.second);
    auto *arg = Argument::create(ty.first, param.first, current_function);
    if (!ty.second.empty())
      arg->getType().reference();

    if (!is_external) {
      if (!ty.second.empty()) {
        scopes.insert(arg->name, {ty, arg});
      } else {
        auto *arg_local_addr = Instruction::create(
          OP_Allocate, PredefinedType::IntPtr,
          {ConstantInteger::create(
            calculateArrayTotalLength({PredefinedType::IntPtr, {}}))},
          current_alloca_bb);
        host->createInstruction(OP_Store, PredefinedType::Void,
                                {arg_local_addr, arg}, current_entry_bb);
        scopes.insert(arg->name,
                      {{PredefinedType::IntPtr, {}}, arg_local_addr});
      }
    }
  }
  // Generate Function Body.
  if (!is_external) {
    scopes.enter();
    generateStatement(decl->body);
    scopes.exit();
    current_function->addBasicBlock(return_bb);

    current_alloca_bb->linkByJump(current_entry_bb);
    host->getInsertPoint()->linkByJump(return_bb);
  }
  scopes.exit();
}
} // namespace SyOC