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
  ListIterator(const ListNode<T> *node)
    : m_node(const_cast<ListNode<T> *>(node)) {}

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

  void release(bool free = false);

  ListIterator &release_and_increase(bool free = false);

  ListIterator &release_and_decrease(bool free = false);

  static ListIterator end() { return ListIterator(); }
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
  ListNode<T> head;
  ListNode<T> tail;

  ListNode<T> *dummyHead() { return &head; }
  ListNode<T> *dummyTail() { return &tail; }
  const ListNode<T> *dummyHead() const { return &head; }
  const ListNode<T> *dummyTail() const { return &tail; }

public:
  List() {
    dummyHead()->m_next = dummyTail();
    dummyTail()->m_prev = dummyHead();
  }
  constexpr const_iterator cbegin() const {
    return const_iterator(dummyHead()->m_next);
  }
  constexpr const_iterator cend() const { return const_iterator(dummyTail()); }
  constexpr const_iterator begin() const { return cbegin(); }
  constexpr const_iterator end() const { return cend(); }
  constexpr iterator begin() { return iterator(dummyHead()->m_next); }
  constexpr iterator end() { return iterator(dummyTail()); }

  auto &front() { return *begin(); }
  auto &back() { return *--end(); }
  auto &front() const { return *cbegin(); }
  auto &back() const { return *--cend(); }

  void push_front(ListNode<T> *node);
  void pop_back();
  void pop_front();
  void push_back(ListNode<T> *node);

  [[nodiscard]] constexpr bool empty() const {
    return dummyHead()->m_next == dummyTail();
  }
  [[nodiscard]] constexpr size_t size() const {
    size_t ret = 0;
    for (auto iter = cbegin(); iter != cend(); ++iter) ++ret;
    return ret;
  }
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

  const_pointer cast_to_derived(auto *p) const {
    return static_cast<const_pointer>(p);
  }
  const_pointer cast_to_derived() const { return cast_to_derived(this); }
  pointer cast_to_derived(auto *p) { return static_cast<pointer>(p); }
  pointer cast_to_derived() { return cast_to_derived(this); }
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

  void release(bool free = false) {
    remove_from_list();
    if (free)
      delete this;
  }
};

template <typename T> void List<T>::push_front(ListNode<T> *node) {
  dummyHead()->insert_after(node->cast_to_derived());
}

template <typename T> void List<T>::push_back(ListNode<T> *node) {
  dummyTail()->insert_before(node->cast_to_derived());
}

template <typename T> void List<T>::pop_back() {
  if (empty())
    throw std::runtime_error("pop_back() on empty list");
  back().release(true);
}

template <typename T> void List<T>::pop_front() {
  if (empty())
    throw std::runtime_error("pop_front() on empty list");
  front().release(true);
}

template <typename T> void ListIterator<T>::release(bool free) {
  m_node->release(free);
  m_node = nullptr;
}

template <typename T>
ListIterator<T> &ListIterator<T>::release_and_increase(bool free) {
  ListIterator tmp(*this);
  ++(*this);
  tmp.release(free);
  return *this;
}

template <typename T>
ListIterator<T> &ListIterator<T>::release_and_decrease(bool free) {
  ListIterator tmp(*this);
  --(*this);
  tmp.release(free);
  return *this;
}