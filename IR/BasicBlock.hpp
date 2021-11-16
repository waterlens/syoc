#pragma once

#include "Instruction.hpp"

struct BasicBlock {
  size_t id;
  Instruction *begin;
  Instruction *end;
  std::string get_id_string() const {
    return fmt::format("{}", id);
  }
  std::string to_string() const;
};