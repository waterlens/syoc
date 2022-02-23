#pragma once

#include <cstddef>

#if defined(_WIN32)
struct RuntimeStack {
  static int hard_max_size() { return 0; }
  static int soft_max_size() { return 0; }
  static void set_max_size(int sz) {}
};
#else
#include <sys/resource.h>
struct RuntimeStack {
  static int hard_max_size() {
    rlimit r;
    getrlimit(RLIMIT_STACK, &r);
    return r.rlim_max;
  }
  static int soft_max_size() {
    rlimit r;
    getrlimit(RLIMIT_STACK, &r);
    return r.rlim_cur;
  }
  static void set_max_size(int sz) {
    rlimit t;
    t.rlim_cur = sz;
    setrlimit(RLIMIT_STACK, &t);
  }
};
#endif