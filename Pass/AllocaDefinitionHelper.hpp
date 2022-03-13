#include "IR/IR.hpp"
#include "Tree/Tree.hpp"

namespace SyOC {
inline std::unordered_set<BasicBlock *>
findDefinitionBlock(Instruction &alloca, bool include_memset = false) {
  assert(alloca.op == OP_Allocate);
  std::unordered_set<BasicBlock *> res;
  for (auto user_iter = alloca.getEdgeHead(); !user_iter.reach_end();
       user_iter++) {
    auto *insn = user_iter->to->as<Instruction *>();
    if (insn->isDefinitionInstruction()) {
      if (!include_memset && insn->op == OP_Memset0)
        continue;
      res.insert(insn->refParent()->as<BasicBlock *>());
    }
  }
  return res;
}
} // namespace SyOC