#include "IR.hpp"
#include "fmt/core.h"
#include "fmt/format.h"
#include "fmt/os.h"
#include <array>
#include <fstream>
#include <stdexcept>

using namespace std;

template <class... Ts> struct overloaded : Ts... { using Ts::operator()...; };
template <class... Ts> overloaded(Ts...) -> overloaded<Ts...>;

void IRBuilder::dumpText() {
  check_module();
  string buffer;
  buffer.reserve(64 * 1024);
  for (auto ct : module->global_constant_table) {
    auto value = module->pool[ct];
    visit(
      overloaded{
        [](auto) { throw runtime_error("unsupported constant value type"); },
        [&](ConstantArray *p) {
          buffer.append(fmt::format("{}\n", p->toString()));
        },
        [&](ConstantExpr *p) {
          buffer.append(fmt::format("{}\n", p->toString()));
        },
      },
      value._);
  }
  for (auto gv : module->global_value_table) {
    auto value = module->pool[gv];
    visit(overloaded{
            [](auto) { throw runtime_error("unsupported global value type"); },
            [&](GlobalVariable *p) {
              buffer.append(fmt::format("global {} @{} : %{}",
                                        p->type.toString(), p->name,
                                        p->identity));
              if (is_handle_valid(p->initializer))
                buffer.append(fmt::format(" = %{}\n", p->initializer));
              else
                buffer.push_back('\n');
            },
          },
          value._);
  }
  for (auto func : module->global_function_table) {
    auto p_func = module->handle_cast<Function *>(func);
    buffer.append(
      fmt::format("fn @{} -> {} (", p_func->name, p_func->ret.toString()));
    for (auto &[name, ty] : p_func->args)
      buffer.append(fmt::format("{} : {}, ", name, ty.toString()));
    if (buffer.ends_with(", "))
      buffer.pop_back(), buffer.pop_back();
    buffer.append("){\n");
    for (auto bb : p_func->basic_block) {
      auto p_bb = module->handle_cast<BasicBlock *>(bb);
      buffer.append(fmt::format("B{}:\n", p_bb->identity));
      for (auto insn : p_bb->insn) {
        auto p_insn = module->handle_cast<Instruction *>(insn);
        buffer.append(fmt::format("    {}\n", p_insn->toString()));
      }
    }
    buffer.append("}\n\n");
  }

  auto out = fmt::output_file("dump.txt");
  out.print("{}", buffer);
}
void IRBuilder::dumpGraph() {
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
    visit(overloaded{[](auto) {},
                     [&](BasicBlock *p) {
                       add_edge(p->parent, p->identity);
                       add_attr(p->identity, {{"label", "bb"}});
                     },
                     [&](Instruction *p) {
                       add_edge(p->parent, p->identity);
                       add_attr(
                         p->identity,
                         {{"label", fmt::format("inst {}", op_name[p->op])}});
                     },
                     [&](GlobalVariable *p) {
                       add_edge(p->parent, p->identity);
                       add_attr(p->identity, {{"label", "global var"}});
                     },
                     [&](ConstantArray *p) {
                       add_edge(p->parent, p->identity);
                       add_attr(p->identity, {{"label", "const arr"}});
                     },
                     [&](ConstantExpr *p) {
                       add_edge(p->parent, p->identity);
                       add_attr(p->identity, {{"label", "const expr"}});
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