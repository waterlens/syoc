#pragma once

#include <cassert>
#include <cstddef>
#include <stdexcept>
#include <type_traits>

template <typename T> class TrivialValueListNode;

template <typename T> class TrivialValueListIterator {
private:
  TrivialValueListNode<T> *m_node;

  T *cast_to_derived() {
    return static_cast<T *>(m_node);
  }
public:
  using value_type = T;
  using reference = value_type &;
  using const_reference = const value_type &;
  using pointer = value_type *;
  using const_pointer = const value_type *;

  TrivialValueListIterator() : m_node(nullptr){};
  TrivialValueListIterator(TrivialValueListNode<T> *node) : m_node(node) {}

  auto *base() const { return m_node; }
  auto *base() { return m_node; }

  TrivialValueListIterator &operator++() {
    m_node = m_node->m_next;
    return *this;
  }

  TrivialValueListIterator operator++(int) {
    TrivialValueListIterator tmp(*this);
    ++(*this);
    return tmp;
  }

  TrivialValueListIterator &operator--() {
    m_node = m_node->m_prev;
    return *this;
  }

  TrivialValueListIterator operator--(int) {
    TrivialValueListIterator tmp(*this);
    --(*this);
    return tmp;
  }

  bool operator==(const TrivialValueListIterator &rhs) const {
    return m_node == rhs.m_node;
  }

  bool operator!=(const TrivialValueListIterator &rhs) const {
    return m_node != rhs.m_node;
  }

  const_reference operator*() const { return *m_node; }
  const_pointer operator->() const { return m_node; }
  reference operator*() { return *cast_to_derived(); }
  pointer operator->() { return cast_to_derived(); }
};

template <typename T> class TrivialValueList {
public:
  using value_type = T;
  using reference = value_type &;
  using const_reference = const value_type &;
  using pointer = value_type *;
  using const_pointer = const value_type *;
  using iterator = TrivialValueListIterator<T>;
  using const_iterator = TrivialValueListIterator<T>;

protected:
  TrivialValueListNode<T> *head = nullptr;
  TrivialValueListNode<T> *tail = nullptr;

  void addToEmptyList(TrivialValueListNode<T> *node) {
    head = node;
    tail = node;
    head->m_prev = nullptr;
    tail->m_next = nullptr;
  }

public:
  constexpr const_iterator cbegin() const { return const_iterator(head); }
  constexpr const_iterator cend() const { return const_iterator(); }
  constexpr const_iterator begin() const { return cbegin(); }
  constexpr const_iterator end() const { return cend(); }
  constexpr iterator begin() { return iterator(head); }
  constexpr iterator end() { return iterator(); }

  auto &front() { return *iterator(head); }
  auto &back() { return *iterator(tail); }
  auto &front() const { return *const_iterator(head); }
  auto &back() const { return *const_iterator(tail); }

  void push_front(TrivialValueListNode<T> *node) {
    if (empty()) {
      addToEmptyList(node);
    } else {
      assert(node->m_prev == nullptr);
      assert(node->m_next == nullptr);
      node->m_next = head;
      head->m_prev = node;
      head = node;
    }
  }

  void push_back(TrivialValueListNode<T> *node) {
    if (empty()) {
      assert(tail == nullptr);
      addToEmptyList(node);
    } else {
      assert(node->m_prev == nullptr);
      assert(node->m_next == nullptr);
      node->m_prev = tail;
      tail->m_next = node;
      tail = node;
    }
  }

  [[nodiscard]] constexpr bool empty() const { return head == nullptr; }
};

template <typename T> class TrivialValueListNode {
public:
  using value_type = T;
  using pointer = value_type *;
  using const_pointer = const value_type *;

protected:
  TrivialValueListNode *m_next;
  TrivialValueListNode *m_prev;
  friend TrivialValueListIterator<T>;
  friend TrivialValueList<T>;

public:
  constexpr const_pointer next() const { return m_next; }
  constexpr pointer next() { return m_next; }
  constexpr const_pointer prev() const { return m_prev; }
  constexpr pointer prev() { return m_prev; }
  constexpr void insert_before(pointer node) {
    if (node) {
      if (m_prev)
        m_prev->m_next = node;
      node->m_next = this;
      node->m_prev = m_prev;
      m_prev = node;
    }
  }

  constexpr void insert_after(pointer node) {
    if (node) {
      if (m_next)
        m_next->m_prev = node;
      node->m_next = m_next;
      node->m_prev = this;
      m_next = node;
    }
  }

  constexpr void extract() {
    if (m_prev)
      m_prev->m_next = m_next;
    if (m_next)
      m_next->m_prev = m_prev;
    m_prev = nullptr;
    m_next = nullptr;
  }
};
