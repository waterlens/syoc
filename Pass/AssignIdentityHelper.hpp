#pragma once

#include "IR/YIR.hpp"
#include <algorithm>

namespace YIR {

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

} // namespace YIR