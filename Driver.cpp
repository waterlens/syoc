#include "Parser/Parser.hpp"
#include "Pass/PassCollection.hpp"
#include "Pass/PromoteMem2Reg.hpp"
#include "Transformer/Transformer.hpp"
#include "Tree/Tree.hpp"
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
                Option<std::string_view>("filename"));

  optParser.parse(argc, argv);

  if (optParser["-h"].as<bool>()) {
    fmt::print("usage: syoc [filename] [-h | --help] [-v | --version]\n");
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
int getint(), getch(), getarray(int a[]);
void putint(int a), putch(int a), putarray(int n, int a[]);
void starttime();
void stoptime();
)" + fileContent;

  SyOC::Parser parser(fileContent);
  parser.tokenize();
  auto *tree = parser.parse();
  SyOC::Transformer transformer(tree);
  transformer
    .doTreeTransformation<SyOC::ConstantInitializerFold, SyOC::TypeCheck>();
  transformer.doTree2SSATransformation<SyOC::Tree2SSA>();
  transformer
    .doSSATransformation<SyOC::SimplifyCFG, SyOC::SimpleAllocationElimination,
                         SyOC::IRDump, SyOC::IDominatorDump, SyOC::IDFDump,
                         SyOC::PromoteMem2Reg, SyOC::IRDump, SyOC::CFGDump>();
  return 0;
}
