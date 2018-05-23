#ifndef simmer__activity_utils_functions_h
#define simmer__activity_utils_functions_h

#include <simmer/common.h>

namespace simmer { namespace internal {

  template <typename T>
  Fn<T(T, T)> get_op(char mod) {
    switch(mod) {
    case '+':
      return BIND(std::plus<double>(), _1, _2);
    case '*':
      return BIND(std::multiplies<double>(), _1, _2);
    default:
      Rcpp::stop("operator '%c' not supported", mod);
    }
    return NULL;
  }

}} // namespace internal simmer

#endif
