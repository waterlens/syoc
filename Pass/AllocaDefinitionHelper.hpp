#include "IR/IR.hpp"

namespace SyOC {
inline std::unordered_set<BasicBlock *>
findDefinitionBlock(Instruction &alloca) {
  assert(alloca.op == OP_Allocate);
  std::unordered_set<BasicBlock *> res;
  for (auto user_iter = alloca.getEdgeHead(); !user_iter.reach_end();
       user_iter++) {
    auto *insn = user_iter->to->as<Instruction *>();
    if (insn->isDefinitionInstruction())
      res.insert(insn->getParent()->as<BasicBlock *>());
  }
  return res;
}
} // namespace SyOC