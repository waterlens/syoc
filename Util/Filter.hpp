#pragma once

#include <iterator>

template <typename Predicate, typename Iterator> class filter_iterator {
public:
  using value_type = typename std::iterator_traits<Iterator>::value_type;
  using reference = typename std::iterator_traits<Iterator>::reference;
  using pointer = typename std::iterator_traits<Iterator>::pointer;
  using difference_type =
    typename std::iterator_traits<Iterator>::difference_type;

  filter_iterator();
  filter_iterator(Predicate f, Iterator x, Iterator end = Iterator())
    : m_pred(f), m_iter(x), m_end(end) {
    moveToSatisfy();
  }
  filter_iterator(Iterator x, Iterator end = Iterator())
    : m_pred(), m_iter(x), m_end(end) {
    moveToSatisfy();
  }
  constexpr Predicate predicate() const { return m_pred; }
  constexpr Iterator end() const { return m_end; }
  constexpr Iterator const &base() const { return m_iter; }
  constexpr reference operator*() const { return *m_iter; }
  constexpr pointer operator->() const { return std::addressof(operator*()); }
  constexpr filter_iterator &operator++() {
    if (m_iter == m_end)
      return *this;
    ++m_iter;
    moveToSatisfy();
    return *this;
  }
  constexpr filter_iterator operator++(int) {
    auto cur = *this;
    ++(*this);
    return cur;
  }
  constexpr bool operator==(filter_iterator rhs) const {
    return m_iter == rhs.m_iter && m_end == rhs.m_end;
  }
  constexpr bool operator!=(filter_iterator rhs) const {
    return !this->operator==(rhs);
  }

private:
  constexpr void moveToSatisfy() {
    while (m_iter != m_end && !static_cast<bool>(m_pred(operator*()))) ++m_iter;
  }
  Predicate m_pred;
  Iterator m_iter;
  Iterator m_end;
};

template <typename Iterator> class filter_range {
public:
  filter_range(Iterator begin, Iterator end) : m_begin(begin), m_end(end){};
  constexpr Iterator begin() const { return m_begin; }
  constexpr Iterator end() const { return m_end; }
  constexpr Iterator cbegin() const { return m_begin; }
  constexpr Iterator cend() const { return m_end; }
  constexpr auto &front() { return *begin(); }
  constexpr bool empty() { return m_begin == m_end; }
  constexpr size_t size() { return std::distance(m_begin, m_end); }

private:
  Iterator m_begin;
  Iterator m_end;
};

template <typename T, typename Predicate> auto filter(T &t, Predicate f) {
  return filter_range(filter_iterator(f, std::begin(t), std::end(t)),
                      filter_iterator(f, std::end(t), std::end(t)));
}

template <typename T, typename Predicate>
auto const_filter(const T &t, Predicate f) {
  return filter_range(filter_iterator(f, std::cbegin(t), std::cend(t)),
                      filter_iterator(f, std::cend(t), std::cend(t)));
}

template <typename T, typename Predicate>
auto reverse_filter(T &t, Predicate f) {
  return filter_range(filter_iterator(f, std::rbegin(t), std::rend(t)),
                      filter_iterator(f, std::rend(t), std::rend(t)));
}

template <typename T, typename Predicate>
auto const_reverse_filter(const T &t, Predicate f) {
  return filter_range(filter_iterator(f, std::crbegin(t), std::crend(t)),
                      filter_iterator(f, std::crend(t), std::crend(t)));
}