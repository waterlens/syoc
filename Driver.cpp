#include "Parser/Parser.hpp"
#include <fmt/core.h>
#include <fstream>

using namespace std;

int main() {
  string fileContent;
  getline(ifstream(".\\Test\\brainfuck-bootstrap.sy"), fileContent, '\0');

  Parser parser(fileContent);
  parser.tokenize();
  for (auto &&s : parser.tokens)
    fmt::print("{}\n", s.text);

  return 0;
}