#include "Parser/Parser.hpp"
#include "Tree/Tree.hpp"
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

  Parser parser(fileContent);
  parser.tokenize();
  auto p = parser.parse();
  
  return 0;
}