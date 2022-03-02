#pragma once

#include "IR/IR.hpp"
#include <algorithm>
#include <cassert>

namespace SyOC {

inline void clearVisited(Function *f) {
  for (auto &bb : f->block) bb.refVisited() = false;
}

template <bool Postfix>
inline void dfs(BasicBlock *bb, std::vector<BasicBlock *> &out) {
  bb->refVisited() = true;
  if constexpr (!Postfix)
    out.push_back(bb);
  for (auto &e : bb->getSuccessor()) {
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
  clearVisited(f);
  std::vector<BasicBlock *> ret;
  dfs<Postfix>(&block.front(), ret);
  if constexpr (Reverse)
    std::reverse(ret.begin(), ret.end());
  clearVisited(f);
  return ret;
}

} // namespace SyOC