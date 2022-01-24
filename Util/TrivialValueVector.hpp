#pragma once

#include <cstddef>
#include <stdexcept>
#include <type_traits>

template <typename T, size_t DefaultSize = 4,
          typename = typename std::enable_if<std::is_trivial_v<T>>::type>
class TrivialValueVector {
public:
  using value_type = T;
  using reference = T &;
  using const_reference = const T &;
  using pointer = T *;
  using const_pointer = const T *;
  using iterator = T *;
  using const_iterator = const T *;
  using size_type = unsigned;
  using difference_type = std::ptrdiff_t;

private:
  unsigned m_capacity;
  unsigned m_size;
  T *m_access_ptr;
  T m_small_data[DefaultSize];
  bool m_heap_allocated;

private:
  constexpr void grow() {
    auto new_capacity = m_capacity + m_capacity / 2;
    T *new_data = new T[new_capacity];
    for (unsigned i = 0; i < m_size; ++i) new_data[i] = m_access_ptr[i];
    if (m_heap_allocated)
      delete[] m_access_ptr;
    else
      m_heap_allocated = true;
    m_access_ptr = new_data;
    m_capacity = new_capacity;
  }

  constexpr void copy_another(const TrivialValueVector &v) {
    if (!v.m_heap_allocated) {
      m_capacity = DefaultSize;
      m_size = v.m_size;
      m_access_ptr = m_small_data;
      m_heap_allocated = false;
    } else {
      m_capacity = v.m_capacity;
      m_size = v.m_size;
      m_access_ptr = new T[m_capacity];
      m_heap_allocated = true;
    }
    for (unsigned i = 0; i < m_size; ++i) m_access_ptr[i] = v.m_access_ptr[i];
  }

public:
  TrivialValueVector()
    : m_capacity(DefaultSize), m_size(), m_access_ptr(m_small_data),
      m_heap_allocated(false) {}
  ~TrivialValueVector() {
    if (m_heap_allocated)
      delete[] m_access_ptr;
  }
  TrivialValueVector(const TrivialValueVector &v) { copy_another(v); }
  TrivialValueVector &operator=(const TrivialValueVector &v) {
    if (m_heap_allocated)
      delete[] m_access_ptr;
    copy_another(v);
  }
  const T &operator[](unsigned i) const { return m_access_ptr[i]; }
  const T &at(unsigned i) const {
    if (i >= m_size)
      throw std::out_of_range("TrivialValueVector::at");
    return m_access_ptr[i];
  }
  T &operator[](unsigned i) { return m_access_ptr[i]; }
  T &at(unsigned i) {
    if (i >= m_size)
      throw std::out_of_range("TrivialValueVector::at");
    return m_access_ptr[i];
  }
  constexpr void push_back(const T &value) {
    if (m_size == m_capacity)
      grow();
    m_access_ptr[m_size++] = value;
  }
  constexpr void pop_back() {
    if (m_size)
      m_size--;
  }
  constexpr void pop_front() {
    for (int i = 0; i < m_size - 1; ++i) m_access_ptr[i] = m_access_ptr[i + 1];
    if (m_size)
      m_size--;
  }
  constexpr bool empty() const noexcept { return size() == 0; }
  constexpr unsigned size() const noexcept { return m_size; }
  constexpr iterator begin() noexcept { return m_access_ptr; }
  constexpr iterator end() noexcept { return m_access_ptr + m_size; }
  constexpr const_iterator cbegin() const noexcept { return m_access_ptr; }
  constexpr const_iterator cend() const noexcept {
    return m_access_ptr + m_size;
  }
};