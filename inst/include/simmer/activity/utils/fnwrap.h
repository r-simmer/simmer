#ifndef simmer__activity_utils_fnwrap_h
#define simmer__activity_utils_fnwrap_h

#include <simmer.h>

template <typename T, typename U, typename V>
class FnWrap {
public:
  FnWrap() {}
  FnWrap(const Fn<T(U)>& call, const V& arg) : call(call), arg(arg) {}

  T operator()(U param) { return call(param); }

  friend std::ostream& operator<<(std::ostream& out, const FnWrap<T, U, V>& fn) {
    out << fn.arg;
    return out;
  }

private:
  Fn<T(U)> call;
  V arg;
};

#endif
