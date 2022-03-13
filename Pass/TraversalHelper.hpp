#pragma once

#include "IR/IR.hpp"
#include <algorithm>
#include <cassert>
#include <queue>
#include <vector>

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
inline std::vector<BasicBlock *> dfsTraversal(Function *f) {
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

template <bool Reverse>
inline std::vector<BasicBlock *> bfsTraversal(Function *f) {
  if (f == nullptr || f->refExternal())
    return {};
  auto &block = f->block;
  assert(!block.empty());
  clearVisited(f);
  std::vector<BasicBlock *> ret;
  std::queue<BasicBlock *> worklist;

  worklist.push(&block.front());

  while (!worklist.empty()) {
    auto *node = worklist.front();
    worklist.pop();

    ret.push_back(node);

    for (auto &e : node->getSuccessor()) {
      if (!e.to->refVisited()) {
        worklist.push(e.to);
        e.to->refVisited() = true;
      }
    }
  }

  if constexpr (Reverse)
    std::reverse(ret.begin(), ret.end());
  clearVisited(f);
  return ret;
}

} // namespace SyOC