#pragma once

#include "IR/IR.hpp"
#include "IR/ASM.hpp"
#include <algorithm>

namespace SyOC {

inline void assignIdentity(IRHost &host) {
  unsigned long long id = 0;
  unsigned long long fid = 0;
  std::for_each(
    host.getModule()->func.begin(), host.getModule()->func.end(), [&](auto &f) {
      f->getIdentity() = fid++;
      std::for_each(f->block.begin(), f->block.end(), [&](auto &bb) {
        bb.getIdentity() = ++id;
        std::for_each(bb.begin(), bb.end(),
                      [&](auto &insn) { insn.getIdentity() = ++id; });
      });
    });
}

namespace ARMv7a {

inline void assignMIdentity(MInstHost &host) {
  size_t id = 0;
  size_t bbid = 0;
  for (MFunction *func : host.root->function) {
    std::for_each(func->block.begin(), func->block.end(), [&](auto &mbb) {
      mbb.id = bbid++;
      std::for_each(mbb.insn.begin(), mbb.insn.end(), [&](auto &minst) {
        minst.id = id++;
      });
    });
  }
}

} // namespace ARMv7a
} // namespace SyOC