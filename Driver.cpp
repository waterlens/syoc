#include "Parser/Parser.hpp"
#include "Transformer/Pass.hpp"
#include "Transformer/Transformer.hpp"
#include "Tree/Tree.hpp"
#include "Util/TrivialValueVector.hpp"
#include <fmt/core.h>
#include <fstream>

using namespace std;

int main(int argc, char *argv[]) {
  string fileName;
  string fileContent;
  if (argc > 1)
    fileName = argv[1];
  else
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
  auto tree = parser.parse();
  Transformer transformer(tree);
  transformer.registerTreeTransformation(
    {{"Constant Initializer Fold", ConstantInitializerFold{}},
     {"Type Check", TypeCheck{}}});
  transformer.registerTree2SSATransformation(Tree2SSA{});
  transformer.transform();
  return 0;
}