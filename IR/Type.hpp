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

  bool isDeclarationSpecifiersValid() {
    switch (ty_spec) {
    case INT:
    case VOID:
      return true;
    default:
      return false;
    }
  }

  bool isVoid() { return ty_spec & VOID; }

  std::string_view to_string() const {
    if (ty_qual == CONST) {
      return std::string_view(ty_spec & INT ? "const int" : "const void");
    } else {
      return std::string_view(ty_spec & INT ? "int" : "void");
    }
  }
};