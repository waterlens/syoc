#include <string>
#include <vector>

template <typename Iterator>
std::string join(Iterator begin, Iterator end, std::string_view delimiter) {
  std::string result;
  for (auto it = begin; it != end; ++it) {
    if (it != begin)
      result += delimiter;
    result += *it;
  }
  return result;
}

template <typename Iterator, typename Printer>
std::string join(Iterator begin, Iterator end, Printer printer,
                 std::string_view delimiter) {
  std::string result;
  for (auto it = begin; it != end; ++it) {
    if (it != begin)
      result += delimiter;
    result += printer(*it);
  }
  return result;
}