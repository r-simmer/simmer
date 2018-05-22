#ifndef simmer__activity_utils_getop_h
#define simmer__activity_utils_getop_h

#include <simmer/common.h>

template <typename T>
Fn<T(T, T)> get_op(char mod) {
  switch(mod) {
  case '+':
    return BIND(std::plus<double>(), _1, _2);
  case '*':
    return BIND(std::multiplies<double>(), _1, _2);
  }
  return NULL;
}

#endif
