#pragma once

#include <cstdlib>
#include <functional>
#include <stdexcept>
#include <string_view>
#include <tuple>
#include <type_traits>
#include <unordered_map>
#include <utility>
#include <vector>

struct OptionName {
  std::string_view long_name;
  std::string_view short_name;
};

template <typename T> class Option {
  OptionName name;
  std::string_view default_value;
  bool has_default_value;
  Option() = default;

public:
  using ValueType = T;
  Option(std::string_view long_name)
    : name{long_name, {}}, has_default_value(false) {}
  Option(std::string_view long_name, std::string_view short_name)
    : name{long_name, short_name}, has_default_value(false) {}
  Option setDefault(std::string_view s) {
    default_value = s;
    has_default_value = true;
    return *this;
  }
  std::string_view getLongName() { return name.long_name; }
  std::string_view getShortName() { return name.short_name; }
  std::string_view getDefault() { return default_value; }
  bool hasDefault() { return has_default_value; }
};

class OptionProxy {
  const std::string_view value;

public:
  OptionProxy(std::string_view v) : value(v) {}
  template <typename T> T as() const {
    if constexpr (std::is_same_v<T, bool>) {
      return value == "true" || value == "True";
    } else if constexpr (std::is_integral_v<T>) {
      // argv must be zero terminated, so it's safe here
      return (T)strtoll(value.data(), nullptr, 0);
    } else if constexpr (std::is_same_v<T, std::string_view>) {
      return value;
    }
  }
};

class OptionParser {
  // name |-> idx, hasDefault, initialized, type
  using OptionConfig =
    std::unordered_map<std::string_view, std::tuple<unsigned, bool, int>>;
  OptionConfig long_option, short_option;
  std::unordered_map<std::string_view, unsigned> positional_option;
  std::vector<std::pair<std::string_view, bool>> raw_storage;
  std::vector<std::string_view> positional_storage;
  static std::tuple<std::string_view, std::string_view, bool>
  split_arg(std::string_view arg) {
    auto pos = arg.find_first_of('=');
    if (pos == std::string_view::npos)
      return {arg, {}, false};
    return {arg.substr(0, pos), arg.substr(pos + 1), true};
  }

public:
  template <typename T> void add(T opt) {
    std::string_view l = opt.getLongName();
    std::string_view s = opt.getShortName();
    int ty;
    if constexpr (std::is_same_v<typename T::ValueType, bool>)
      ty = 0;
    else
      ty = 1;
    if (l.starts_with('-')) {
      if (long_option.contains(l))
        throw std::invalid_argument("duplicate long option");
      long_option[l] = {raw_storage.size(), opt.hasDefault(), ty};
      if (!s.empty()) {
        if (short_option.contains(s))
          throw std::invalid_argument("duplicate short option");
        short_option[s] = {raw_storage.size(), opt.hasDefault(), ty};
      }
      if (l.length() || s.length())
        raw_storage.emplace_back(opt.getDefault(), false);
    } else if (!l.empty()) {
      if (positional_option.contains(l))
        throw std::invalid_argument("duplicate positional option");
      positional_option[l] = positional_storage.size();
      positional_storage.emplace_back();
    }
  }
  template <typename Head, typename... Tail> void add(Head h, Tail... t) {
    add(h);
    add(t...);
  }

  void parse(int argc, char *argv[]) {
    int positional_count = 0;
    for (int i = 1; i < argc; ++i) {
      std::string_view arg{argv[i]};
      OptionConfig::mapped_type *config_value;
      if (arg.starts_with('-')) { // named option
        auto [name, _value, has_value] = split_arg(arg);
        auto value = _value;
        if (long_option.contains(name))
          config_value = &long_option.at(name);
        else if (short_option.contains(name))
          config_value = &short_option.at(name);
        else
          throw std::invalid_argument("no such an option");
        auto &[idx, has_default, val_ty] = *config_value;
        if (!has_value) {
          if (val_ty != 0) {
            if (i + 1 >= argc)
              throw std::invalid_argument("options are not enough");
            value = argv[i + 1];
            i++;
          } else
            value = "true";
        }
        raw_storage[idx] = {value, true};
      } else { // positional option
        positional_storage[positional_count++] = arg;
      }
    }
  }
  const std::vector<std::string_view> &getAllPositionalOptions() const {
    return positional_storage;
  }
  OptionProxy operator[](const std::string_view &name) const {
    if (name.starts_with('-')) {
      const OptionConfig::mapped_type *config_value;
      if (long_option.contains(name))
        config_value = &long_option.at(name);
      else if (short_option.contains(name))
        config_value = &short_option.at(name);
      else
        throw std::invalid_argument("no such an option");
      const auto &[idx, has_default, val_ty] = *config_value;
      auto storage = raw_storage.at(idx);
      if (!(has_default || storage.second))
        throw std::invalid_argument("required option is not supplied");
      return {storage.first};
    }
    if (positional_option.contains(name))
      return {positional_storage.at(positional_option.at(name))};
    throw std::invalid_argument("no such a positional option");
  }
  OptionProxy operator[](size_t i) const { return {positional_storage.at(i)}; }
};