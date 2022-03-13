#include "DeadCodeElimination.hpp"
#include "IR/IR.hpp"

namespace SyOC {
void DeadCodeElimination::removeDeadInstruction(IRHost &host) {
  for (auto *func : host.getModule()->func) {
    if (func->refExternal())
      continue;
    for (auto &block : func->block)
      for (auto insn_iter = block.begin(); insn_iter != block.end();) {
        if (insn_iter->safeEliminative() && insn_iter->hasNoEdge())
          insn_iter.release_and_increase(true);
        else
          ++insn_iter;
      }
  }
}
} // namespace SyOC
