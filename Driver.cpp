#include "IR/IR.hpp"
#include "Parser/Parser.hpp"
#include "Pass/NewPass/Dump.hpp"
#include "Pass/NewPass/Tree2SSA.hpp"
#include "Pass/PassCollection.hpp"
#include "Pass/SimpleAllocationElimination.hpp"
#include "Pass/UseAnalysis.hpp"
#include "Transformer/Transformer.hpp"
#include "Tree/Tree.hpp"
#include "Util/OptionParser.hpp"
#include "Util/TrivialValueVector.hpp"
#include <fmt/core.h>
#include <fstream>
#include <string_view>


using namespace std;

int main(int argc, char *argv[]) {
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

  Parser parser(fileContent);
  parser.tokenize();
  auto *tree = parser.parse();
  YIR::Transformer transformer(tree);
  Transformer transformer2(tree);
  transformer.doTreeTransformation<ConstantInitializerFold, TypeCheck>();
  transformer.doTree2SSATransformation<YIR::Tree2SSA>();
  transformer.doSSATransformation<YIR::IRDump>();
  //transformer2.doTree2SSATransformation<Tree2SSA>();
  //transformer2.doSSATransformation<UseAnalysis, BBPredSuccAnalysis, IRDump, CFGDump>();
  return 0;
}
