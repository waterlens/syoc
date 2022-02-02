#pragma once

#include <stdexcept>
#include <string_view>
#include <tuple>
#include <type_traits>
#include <utility>
#include <vector>

enum class OptionType {
  Named,
  Positional,
};

using OptT = OptionType;

template <typename T, OptionType OT> class Option {
  std::string_view long_name;
  std::string_view short_name;
  T default_value;
  T real_value;
  OptionType opt_type;
  bool has_default_value;
  bool has_real_value = false;
  Option() {}

public:
  Option(std::string_view long_name)
    : opt_type(OT), long_name(long_name), has_default_value(false) {}
  Option(std::string_view long_name, std::string_view short_name)
    : opt_type(OT), Option(false), long_name(long_name), short_name(short_name),
      has_default_value(false) {}
  Option(std::string_view long_name, T &&default_value)
    : opt_type(OT), Option(false), long_name(long_name),
      default_value(std::forward(default_value)), has_default_value(true) {}
  Option(std::string_view long_name, std::string_view short_name,
         T &&default_value)
    : opt_type(OT), Option(false), long_name(long_name), short_name(short_name),
      default_value(std::forward(default_value)), has_default_value(true) {}
};

class OptionParser {

public:
  template <typename T> void add(T &&opt) {
  }
  template <typename Head, typename... Tail> void add(Head &&h, Tail &&...c) {
    add(std::forward<Head>(h));
    add(std::forward<Tail>(c)...);
  }
  void parse(int argc, char *argv[]) {
    for (int i = 0; i < argc; ++i) {
    }
  }
  template <typename T> T operator[](const std::string_view &name) const {}
};