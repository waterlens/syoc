#pragma once

#include <cassert>
#include <cstddef>
#include <stdexcept>
#include <type_traits>

template <typename T> class ListNode;

template <typename T> class ListIterator {
private:
  ListNode<T> *m_node;

  T *cast_to_derived() { return static_cast<T *>(m_node); }

public:
  using value_type = T;
  using reference = value_type &;
  using const_reference = const value_type &;
  using pointer = value_type *;
  using const_pointer = const value_type *;

  ListIterator() : m_node(nullptr){};
  ListIterator(ListNode<T> *node) : m_node(node) {}

  auto *base() const { return cast_to_derived(); }
  auto *base() { return cast_to_derived(); }

  ListIterator &operator++() {
    m_node = m_node->m_next;
    return *this;
  }

  ListIterator operator++(int) {
    ListIterator tmp(*this);
    ++(*this);
    return tmp;
  }

  ListIterator &operator--() {
    m_node = m_node->m_prev;
    return *this;
  }

  ListIterator operator--(int) {
    ListIterator tmp(*this);
    --(*this);
    return tmp;
  }

  bool operator==(const ListIterator &rhs) const {
    return m_node == rhs.m_node;
  }

  bool operator!=(const ListIterator &rhs) const {
    return m_node != rhs.m_node;
  }

  const_reference operator*() const { return *m_node; }
  const_pointer operator->() const { return m_node; }
  reference operator*() { return *cast_to_derived(); }
  pointer operator->() { return cast_to_derived(); }

  void release(bool free = false) {
    m_node->remove_from_list();
    if (free)
      delete m_node;
    m_node = nullptr;
  }

  ListIterator &release_and_increase(bool free = false) {
    ListIterator tmp(*this);
    ++(*this);
    tmp.release(free);
    return *this;
  }

  ListIterator &release_and_decrease(bool free = false) {
    ListIterator tmp(*this);
    --(*this);
    tmp.release(free);
    return *this;
  }

  static ListIterator end() { return ListIterator(nullptr); }
};

template <typename T> class List {
public:
  using value_type = T;
  using reference = value_type &;
  using const_reference = const value_type &;
  using pointer = value_type *;
  using const_pointer = const value_type *;
  using iterator = ListIterator<T>;
  using const_iterator = ListIterator<T>;

protected:
  ListNode<T> *head = nullptr;
  ListNode<T> *tail = nullptr;

  void addToEmptyList(ListNode<T> *node) {
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

  void push_front(ListNode<T> *node) {
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

  void pop_back() {
    if (head != nullptr) {
      if (head == tail) {
        iterator(head).release();
        head = nullptr;
        tail = nullptr;
      } else {
        auto iter = iterator(tail);
        iter.release_and_decrease();
        tail = iter.base();
      }
    }
  }

  void pop_front() {
    if (head != nullptr) {
      if (head == tail) {
        iterator(head).release();
        head = nullptr;
        tail = nullptr;
      } else {
        auto iter = iterator(head);
        iter.release_and_increase();
        head = iter.base();
      }
    }
  }

  void push_back(ListNode<T> *node) {
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

template <typename T> class ListNode {
public:
  using value_type = T;
  using pointer = value_type *;
  using const_pointer = const value_type *;

protected:
  ListNode *m_next = nullptr;
  ListNode *m_prev = nullptr;
  friend ListIterator<T>;
  friend List<T>;

  pointer cast_to_derived(auto *p) const { return static_cast<pointer>(p); }
  virtual ~ListNode() = default;

public:
  constexpr const_pointer next() const { return cast_to_derived(m_next); }
  constexpr pointer next() { return cast_to_derived(m_next); }
  constexpr const_pointer prev() const { return cast_to_derived(m_prev); }
  constexpr pointer prev() { return cast_to_derived(m_prev); }
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

  constexpr void remove_from_list() {
    if (m_prev)
      m_prev->m_next = m_next;
    if (m_next)
      m_next->m_prev = m_prev;
    m_prev = nullptr;
    m_next = nullptr;
  }
};
