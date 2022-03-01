#pragma once

#include "IR/YIR.hpp"
#include <algorithm>
#include <cassert>

namespace YIR {

template <bool Postfix>
inline void dfs(BasicBlock *bb, std::vector<BasicBlock *> &out) {
  bb->refVisited() = true;
  if constexpr (!Postfix)
    out.push_back(bb);
  for (auto iter = bb->getSuccessor(); !iter.reach_end(); ++iter) { // NOLINT
    if (!iter->to->refVisited())
      dfs<Postfix>(iter->to, out);
  }
  if constexpr (Postfix)
    out.push_back(bb);
}

template <bool Postfix, bool Reverse>
inline std::vector<BasicBlock *> traversal(Function *f) {
  if (f == nullptr || f->refExternal())
    return {};
  auto &block = f->block;
  assert(!block.empty());
  std::for_each(block.begin(), block.end(),
                [](BasicBlock &bb) { bb.refVisited() = false; });
  std::vector<BasicBlock *> ret;
  dfs<Postfix>(&block.front(), ret);
  if constexpr (Reverse)
    std::reverse(ret.begin(), ret.end());
  return ret;
}

} // namespace YIR