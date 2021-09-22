#pragma once
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
};