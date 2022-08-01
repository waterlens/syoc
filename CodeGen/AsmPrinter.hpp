#pragma once

#include "IR/ASM.hpp"
#include <fmt/core.h>
#include <fmt/format.h>
#include <fmt/os.h>

namespace SyOC {
namespace ARMv7a {

class AsmPrinter {
  std::string buffer;
  std::string filename;
  void dumpMInst(MInstruction *);
  void dumpMBasicBlock(MBasicBlock *);
  void dumpMFunction(MFunction *);
  void dumpGlobalVariable(MInstHost &);
  void dumpAsm(MInstHost &);
public:
  AsmPrinter() = delete;
  AsmPrinter(const std::string& filename) : filename(filename) {}
  AsmPrinter &operator <<(MInstHost &host);
};

}
} // end namespace SyOC
