#pragma once
#include <string_view>

namespace compatibility  {
constexpr bool starts_with(std::string_view sv, std::string_view sv2) noexcept {
  return sv.substr(0, sv2.size()) == sv2;
}
constexpr bool starts_with(std::string_view sv, char c) noexcept {
  return !sv.empty() && sv.front() == c;
}
constexpr bool starts_with(std::string_view sv, const char *s) {
  return starts_with(sv, std::string_view(s));
}
}