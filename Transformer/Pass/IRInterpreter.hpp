#pragma once

#include "IR/IR.hpp"

#include <string_view>

class IRInterpreter {
  std::string_view output_filename;

public:
  IRInterpreter(std::string_view filename) { output_filename = filename; }

  void operator()(IRHost &host) {
    
  }
};