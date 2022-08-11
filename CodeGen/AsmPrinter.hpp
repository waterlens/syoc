#pragma once

#include "IR/ASM.hpp"


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
  AsmPrinter() = default;
  AsmPrinter(const std::string& filename) : filename(filename) {}
  void print(std::string_view outfile, MInstHost *host);
};

}
} // end namespace SyOC
