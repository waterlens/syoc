#pragma once
#include <string_view>
enum {
  VOID = 1 << 0,
  INT = 1 << 4,
};

enum {
  CONST = 1 << 0,
};

struct Type {
  int ty_spec;
  int ty_qual;

  bool isDeclarationSpecifiersValid() const;

  bool isVoid() const;

  std::string_view to_string() const;
};