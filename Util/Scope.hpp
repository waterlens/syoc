#pragma once

#include <vector>
#include <string_view>
#include <unordered_map>

template <typename T> class Scope {
private:
  std::vector<std::unordered_map<std::string_view, T>> scope;

public:
  T find(std::string_view name) {
    for (auto iter = scope.rbegin(); iter != scope.rend(); ++iter) {
      auto result = iter->find(name);
      if (result != iter->end())
        return result->second;
    }
    return T{};
  }
  void enter() { scope.emplace_back(); }
  void exit() { scope.pop_back(); }
  void insert(std::string_view name, const T &value) {
    scope.back().emplace(name, value);
  }
};