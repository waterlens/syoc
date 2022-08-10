#include "Parser/Parser.hpp"
#include "Pass/IRLegalize.hpp"
#include "Pass/DeadCodeElimination.hpp"
#include "Pass/Dump.hpp"
#include "Pass/PassCollection.hpp"
#include "Pass/SimplifyCFG.hpp"
#include "Pass/InstCombine.hpp"
#include "Pass/ConstantFolding.hpp"
#include "Transformer/Transformer.hpp"
#include "Tree/Tree.hpp"
#include "CodeGen/MEISel.hpp"
#include "CodeGen/SimpleRA.hpp"
#include "CodeGen/FrameLowering.hpp"
#include "CodeGen/AsmPrinter.hpp"
#include "Pass/MachineDCE.hpp"
#include "Pass/PeepHole.hpp"
#include "Util/OptionParser.hpp"
#include "Util/RuntimeStackUtil.hpp"
#include "Util/TrivialValueVector.hpp"
#include <fmt/core.h>
#include <fstream>
#include <string_view>

using namespace std;

int main(int argc, char *argv[]) {
  RuntimeStack::set_max_size(RuntimeStack::hard_max_size());
  OptionParser optParser;
  optParser.add(Option<bool>("--help", "-h").setDefault("false"),
                Option<bool>("--version", "-v").setDefault("false"),
                Option<bool>("--debug-opt-parser").setDefault("false"),
                Option<bool>("--implicit-runtime").setDefault("true"),
                Option<bool>("-S").setDefault("true"),
                Option<bool>("-O2").setDefault("true"),
                Option<std::string_view>("--output", "-o"),
                Option<std::string_view>("filename"));

  optParser.parse(argc, argv);

  if (optParser["--debug-opt-parser"].as<bool>()) {
    if (optParser.has("-h"))
      fmt::print("-h: {}\n", optParser["-h"].as<bool>());
    if (optParser.has("-v"))
      fmt::print("-v: {}\n", optParser["-v"].as<bool>());
    if (optParser.has("filename"))
      fmt::print("p[0]: {}\n", optParser["filename"].as<std::string_view>());
    if (optParser.has("-o"))
      fmt::print("-o: {}\n", optParser["-o"].as<std::string_view>());
    return 0;
  }

  if (optParser["-h"].as<bool>()) {
    fmt::print("usage: syoc [filename] [(-o | --output) filename]  [-h | "
               "--help] [-v | --version]\n");
    return 0;
  }

  if (optParser["-v"].as<bool>()) {
    fmt::print("syoc version 0.1.0\n");
    return 0;
  }

  string fileName;
  string fileContent;
  fileName = optParser["filename"].as<std::string_view>();
  if (fileName.empty())
    getline(ifstream("current.txt"), fileName, '\0');

  ifstream inputStream(fileName);
  assert(inputStream.good());
  getline(inputStream, fileContent, '\0');

  if (optParser["--implicit-runtime"].as<bool>()) {
    fileContent = R"(
int getint(),getch(),getarray(int a[]);
float getfloat();
int getfarray(float a[]);

void putint(int a),putch(int a),putarray(int n,int a[]);
void putfloat(float a);
void putfarray(int n, float a[]);

void starttime();
void stoptime();
int __aeabi_idivmod(int a, int b);
int __aeabi_idiv(int a, int b);
)" + fileContent;
  }

  SyOC::Parser parser(fileContent);
  parser.tokenize();
  auto *tree = parser.parse();
  SyOC::Transformer transformer(tree);
  transformer
    .doTreeTransformation<SyOC::ConstantInitializerFold, SyOC::TypeCheck>();
  // from tree gen ssa ir
  transformer.doTree2SSATransformation<SyOC::Tree2SSA>();
  // opt passes
  transformer
    .doSSATransformation<SyOC::SimplifyCFG,
                         SyOC::SimpleAllocationElimination,
                         SyOC::IRDump,
                         // SyOC::PromoteMem2Reg,
                         SyOC::InstCombine, SyOC::ConstantFolding, SyOC::SimplifyCFG,
                         SyOC::SimpleAllocationElimination,
                         SyOC::DeadCodeElimination,
                         SyOC::IRLegalize,
                         SyOC::FixTimeMeasurement,
                         SyOC::IRDump,
                         SyOC::CFGDump>();
  // instruction selection
  SyOC::ARMv7a::AsmPrinter out;
  static int asm_count = 0;
  transformer.doSSA2MInstTransformation<SyOC::MEISel>();
  out.print("mir.s", transformer.getMIR());

  transformer.doMInstTransformation<SyOC::ARMv7a::SimpleRA,
                                    SyOC::ARMv7a::PeepHole,
                                    SyOC::ARMv7a::MachineDCE,
                                    SyOC::ARMv7a::FrameLowering
                                    >();
  std::string asmFileName;
  if (optParser.has("-o")) {
    asmFileName = optParser["-o"].as<std::string_view>();
  } else {
    asmFileName = fmt::format("dump-asm-{:d}.s", asm_count);
  }
  out.print(asmFileName, transformer.getMIR());
  return 0;
}
