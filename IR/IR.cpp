#include "IR.hpp"
#include "fmt/core.h"
#include "fmt/os.h"
#include <array>

using namespace std;

template <> ConstantArray *Constant::cast<ConstantArray *>() {
  if (constant_type == CT_Array) {
    return static_cast<ConstantArray *>(this);
  } else
    throw runtime_error("constant type not match");
}

template <> ConstantInteger *Constant::cast<ConstantInteger *>() {
  if (constant_type == CT_Integer) {
    return static_cast<ConstantInteger *>(this);
  } else
    throw runtime_error("constant type not match");
}

template <> ConstantExpr *Constant::cast<ConstantExpr *>() {
  if (constant_type == CT_Expression) {
    return static_cast<ConstantExpr *>(this);
  } else
    throw runtime_error("constant type not match");
}

template <class... Ts> struct overloaded : Ts... { using Ts::operator()...; };
template <class... Ts> overloaded(Ts...) -> overloaded<Ts...>;
void IRBuilder::dumpAll() {
  check_module();
  string buffer;
  buffer.reserve(64 * 1024);
  auto add_edge = [&](ValueHandle parent, ValueHandle identity) {
    buffer.append(fmt::format("v{} -> v{}\n", parent, identity));
  };
  auto add_attr = [&](ValueHandle id,
                      initializer_list<pair<string_view, string_view>> attr) {
    string attr_list;
    for (auto &[key, value] : attr)
      attr_list.append(fmt::format("{}=\"{}\" ", key, value));
    buffer.append(fmt::format("v{} [{}]\n", id, attr_list));
  };
  for (auto &value : module->pool) {
    // BasicBlock *, Instruction *,
    // GlobalVariable *, Constant *,
    // Function *, Module *
    visit(overloaded{
            [&](BasicBlock *p) {
              add_edge(p->parent, p->identity);
              add_attr(p->identity, {{"label", "bb"}});
            },
            [&](Instruction *p) {
              add_edge(p->parent, p->identity);
              add_attr(p->identity,
                       {{"label", fmt::format("inst {}", op_name[p->op])}});
            },
            [&](GlobalVariable *p) {
              add_edge(p->parent, p->identity);
              add_attr(p->identity, {{"label", "global var"}});
            },
            [&](Constant *p) {
              add_edge(p->parent, p->identity);
              add_attr(p->identity, {{"label", "const"}});
            },
            [&](Function *p) {
              add_edge(p->parent, p->identity);
              add_attr(p->identity, {{"label", "func"}});
            },
            [&](Module *p) {
              add_attr(module_handle, {{"label", "mod"}});
            }},
          value._);
  }

  auto out = fmt::output_file("dump.dot");
  out.print(
    R"(
digraph ir {{
{}
}}
)",
    buffer);
}