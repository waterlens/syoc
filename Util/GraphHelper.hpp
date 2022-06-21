#pragma once

#include <cstddef>
#include <fmt/core.h>
#include <fmt/os.h>
#include <string>
#include <string_view>
#include <vector>

class GraphHelper {
  std::vector<std::tuple<unsigned, unsigned, std::string>> edges;
  std::vector<std::tuple<unsigned, std::string>> nodes;

public:
  GraphHelper() = default;
  void addEdge(unsigned from, unsigned to, const std::string &label) {
    edges.emplace_back(from, to, label);
  }
  void addNode(unsigned node, const std::string &label = "") {
    nodes.emplace_back(node, label);
  }
  void outputToFile(std::string_view path, const char *graph_name) {
    auto out = fmt::buffered_file(path.data(), "wb");
    out.print("digraph {} {{\n  node[shape = box];\n", graph_name);
    for (auto &[node, label] : nodes)
      out.print("  node_{} [label=\"{}\"];\n", node, label);
    for (auto &[from, to, label] : edges)
      out.print("  node_{} -> node_{} [label=\"{}\"];\n", from, to, label);
    out.print("}}\n");
  }
};