#include <string>
#include <vector>

template <typename Iterator>
std::string join(Iterator begin, Iterator end, std::string_view delimiter) {
  std::string result;
  for (auto it =begin; it != end; ++it) {
    if (it != begin)
      result += delimiter;
    result += *it;
  }
  return result;
}