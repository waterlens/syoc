#pragma once

#include "IR/IR.hpp"
#include <algorithm>
#include <cassert>

namespace SyOC {

template <bool Postfix>
inline void dfs(BasicBlock *bb, std::vector<BasicBlock *> &out) {
  bb->refVisited() = true;
  if constexpr (!Postfix)
    out.push_back(bb);
  for (auto &e : bb->getSuccessor()) { // NOLINT
    if (!e.to->refVisited())
      dfs<Postfix>(e.to, out);
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

} // namespace SyOC