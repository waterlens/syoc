#include "IR.hpp"

template <> ConstantArray *Constant::cast<ConstantArray *>() {
  if (constant_type == CT_Array) {
    return static_cast<ConstantArray *>(this);
  } else
    throw std::runtime_error("constant type not match");
}

template <> ConstantInteger *Constant::cast<ConstantInteger *>() {
  if (constant_type == CT_Integer) {
    return static_cast<ConstantInteger *>(this);
  } else
    throw std::runtime_error("constant type not match");
}

template <> ConstantExpr *Constant::cast<ConstantExpr *>() {
  if (constant_type == CT_Expression) {
    return static_cast<ConstantExpr *>(this);
  } else
    throw std::runtime_error("constant type not match");
}