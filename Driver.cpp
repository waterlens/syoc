#include "Parser/Parser.hpp"
#include "Pass/DeadCodeElimination.hpp"
#include "Pass/Dump.hpp"
#include "Pass/PassCollection.hpp"
#include "Pass/SimplifyCFG.hpp"
#include "Pass/InstCombine.hpp"
#include "Transformer/Transformer.hpp"
#include "Tree/Tree.hpp"
#include "CodeGen/MEISel.hpp"
#include "CodeGen/SimpleRA.hpp"
#include "CodeGen/AsmPrinter.hpp"
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
  getline(ifstream(fileName), fileContent, '\0');

  fileContent = R"(
int getint(),getch(),getarray(int a[]);
float getfloat();
int getfarray(float a[]);

void putint(int a),putch(int a),putarray(int n,int a[]);
void putfloat(float a);
void putfarray(int n, float a[]);

void starttime();
void stoptime();
)" + fileContent;

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
                         // SyOC::PromoteMem2Reg,
                         SyOC::InstCombine, SyOC::DeadCodeElimination,
                         SyOC::SimplifyCFG, SyOC::IRDump>();
  // instruction selection
  transformer.doSSA2MInstTransformation<SyOC::MEISel>();
  // transformer.doMInstTransformation<SyOC::ARMv7a::SimpleRA>();
  std::string asmFileName;
  if (optParser.has("-o")) {
    asmFileName = optParser["-o"].as<std::string_view>();
  } else {
    asmFileName = "a.s";
  }
  SyOC::ARMv7a::AsmPrinter out(asmFileName);
  out << *transformer.getMIR();
  return 0;
}
