#pragma once

#include <cassert>
#include <algorithm>
#include <cstddef>
#include <initializer_list>
#include <stdexcept>
#include <type_traits>

template <typename T, size_t DefaultSize = 4,
          typename = std::enable_if_t<std::is_trivial<T>::value>>
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
  size_type m_capacity;
  size_type m_size{};
  T *m_access_ptr;
  T m_small_data[DefaultSize];
  bool m_heap_allocated{};

  constexpr void grow() {
    auto new_capacity = std::max(4U, m_capacity + m_capacity / 2);
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
  TrivialValueVector() : m_capacity(DefaultSize), m_access_ptr(m_small_data) {}
  ~TrivialValueVector() {
    if (m_heap_allocated)
      delete[] m_access_ptr;
  }
  TrivialValueVector(const TrivialValueVector &v) { copy_another(v); }
  TrivialValueVector &operator=(const TrivialValueVector &v) {
    if (this != &v) {
      if (m_heap_allocated)
        delete[] m_access_ptr;
      copy_another(v);
    }
    return *this;
  }
  TrivialValueVector(std::initializer_list<T> l) {
    if (l.size() <= DefaultSize) {
      m_capacity = DefaultSize;
      m_size = l.size();
      m_access_ptr = m_small_data;
      m_heap_allocated = false;
    } else {
      m_capacity = l.size();
      m_size = l.size();
      m_access_ptr = new T[l.size()];
      m_heap_allocated = true;
    }
    auto p = m_access_ptr;
    for (auto &&elem : l) *p++ = elem;
  }
  const_reference operator[](size_type i) const { return m_access_ptr[i]; }
  const_reference at(size_type i) const {
    if (i >= m_size)
      throw std::out_of_range("TrivialValueVector::at");
    return m_access_ptr[i];
  }
  reference operator[](size_type i) { return m_access_ptr[i]; }
  reference &at(size_type i) {
    if (i >= m_size)
      throw std::out_of_range("TrivialValueVector::at");
    return m_access_ptr[i];
  }
  constexpr void push_back(const_reference value) {
    assert(m_size <= m_capacity);
    if (m_size == m_capacity)
      grow();
    m_access_ptr[m_size++] = value;
  }
  template <typename Hook1, typename Hook2>
  constexpr void push_back_with_hook(const_reference value, Hook1 pre_hook, Hook2 post_hook) {
    if (m_size == m_capacity) {
      for (size_t i = 0; i < m_size; ++i) pre_hook(m_access_ptr[i]);
      grow();
      for (size_t i = 0; i < m_size; ++i) post_hook(m_access_ptr[i]);
    }
    m_access_ptr[m_size++] = value;
  }
  constexpr void pop_back() {
    if (m_size != 0U)
      m_size--;
  }
  constexpr void pop_front() {
    if (m_size != 0U) {
      for (unsigned i = 0; i < m_size - 1; ++i)
        m_access_ptr[i] = m_access_ptr[i + 1];
      m_size--;
    }
  }
  [[nodiscard]] constexpr bool empty() const noexcept { return size() == 0; }
  [[nodiscard]] constexpr size_type size() const noexcept { return m_size; }
  constexpr iterator begin() noexcept { return m_access_ptr; }
  constexpr iterator end() noexcept { return m_access_ptr + m_size; }
  constexpr const_iterator begin() const noexcept { return m_access_ptr; }
  constexpr const_iterator end() const noexcept {
    return m_access_ptr + m_size;
  }
  constexpr const_iterator cbegin() const noexcept { return begin(); }
  constexpr const_iterator cend() const noexcept { return end(); }
  constexpr void clear() noexcept { m_size = 0; }
  constexpr reference front() { return *m_access_ptr; }
  constexpr reference back() { return m_access_ptr[m_size - 1]; }
  constexpr const_reference front() const { return *m_access_ptr; }
  constexpr const_reference back() const { return m_access_ptr[m_size - 1]; }
  constexpr void resize(size_type count) {
    if (count > m_size)
      throw std::runtime_error("not supported");
    m_size = count;
  }
};