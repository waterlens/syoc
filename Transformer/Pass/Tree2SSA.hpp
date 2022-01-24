#pragma once

#include "IR/IR.hpp"
#include "Tree/Tree.hpp"

#include <cassert>
#include <numeric>
#include <stdexcept>
#include <vcruntime.h>

class Tree2SSA {
  NodePtr root;
  IRHost *host;
  std::pair<BasicBlock *, SSAValueHandle> global_initializer_block;

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
    if (ty.spec == TS_Int)
      ssa_ty.primitive_type = SSAType::PrimitiveType::Integer;
    else if (ty.spec == TS_Void)
      ssa_ty.primitive_type = SSAType::PrimitiveType::Void;
    for (auto &&dim : ty.dim) {
      ssa_ty.dimension.push_back(dim->as<IntegerLiteral *>()->value);
    }
    return ssa_ty;
  }

  size_t calculateArrayTotalLength(SSAType ty) {
    return std::accumulate(ty.dimension.begin(), ty.dimension.end(), 1,
                           [](auto lhs, auto rhs) { return lhs * rhs; });
  }

  SSAValueHandle generateExpr(ExprPtr init) {
    
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

    if (init->is<InitListExpr *>()) {
      auto l = init->as_unchecked<InitListExpr *>();
      auto [init_len, init_len_handle] =
        host_ref.createConstantInteger(l->values.size());

      if (std::accumulate(l->values.begin(), l->values.end(), true,
                          [](auto lhs, auto rhs) -> bool {
                            return lhs && rhs->template is<IntegerLiteral *>();
                          })) {
        auto [const_arr, arr_handle] = host_ref.createConstantArray();
        for (auto &&e : l->values)
          const_arr->array.push_back(
            e->as_unchecked<IntegerLiteral *>()->value);

        auto [cpy_inst, cpy_handle] =
          host_ref.createIntrinsic("__syoc@memcpy", VoidType);
        cpy_inst->args = {target, arr_handle,
                          l->values.size() < len ? init_len_handle
                                                 : len_handle};
      } else {
        size_t i = 0;
        for (auto &&e : l->values) {
          auto elem_ty = ty;
          elem_ty.dimension.pop_front();
          auto [offset_n, offset_handle] = host_ref.createConstantInteger(i);
          auto [elem, elem_handle] =
            host_ref.createInstruction(OP_Offset, elem_ty, offset_handle);
          generateInitializer(e, elem_ty, elem_handle);
          i++;
        }
      }

    } else {
      auto [init_insn, insn_handle] =
        host_ref.createInstruction(OP_Store, ty, generateExpr(init), target);
    } 
  }

  void generateGlobalVariable(GlobalDeclaration *decl) {
    auto ty = convertType(decl->type);
    ty.indirect_level += 1;
    auto [g, handle] = host->createGlobalVariable();
    g->type = ty;
    g->name = decl->name;
    generateInitializer(decl->initializer, ty, handle);
  }

  void globalGeneration() {
    auto &host_ref = *host;
    setupGlobalInitializerFunction();
    auto module = root->as<Module *>();
    for (auto decl : module->decls) {
      if (decl->is<GlobalDeclaration *>()) {
        auto p = decl->as_unchecked<GlobalDeclaration *>();
        generateGlobalVariable(p);
      } else if (isFunctionDeclaration(decl)) {
        auto f = decl->as_unchecked<FunctionDeclaration *>();
        functionGeneration(f);
      }
    }
  }
  void functionGeneration(FunctionDeclaration *func) {}

public:
  Tree2SSA(){};
  IRHost *operator()(NodePtr tree) {
    host = new IRHost();
    root = tree;
    globalGeneration();
    return host;
  }
};