#ifndef SIMMER_H
#define SIMMER_H

// [[Rcpp::depends(BH)]]
#include <Rcpp.h>

#include <boost/container/set.hpp>
#include <boost/unordered_set.hpp>
#include <boost/unordered_map.hpp>

#define VEC std::vector
#define MSET boost::container::multiset
#define USET boost::unordered_set
#define UMAP boost::unordered_map

#include <boost/lexical_cast.hpp>
#include <boost/optional.hpp>
#include <boost/typeof/typeof.hpp>

#define OPT boost::optional
#define NONE boost::none
#define AUTO BOOST_AUTO

#include <boost/foreach.hpp>

#define foreach_    BOOST_FOREACH
#define foreach_r_  BOOST_REVERSE_FOREACH

#include <boost/variant.hpp>
#include <boost/function.hpp>
#include <boost/bind.hpp>

#define REnv Rcpp::Environment
#define RFn Rcpp::Function
#define Fn boost::function
#define BIND boost::bind

#define FMT(n, justify) std::setw(n) << std::justify
#define IND(n) std::string(n, ' ')

#define PRIORITY_MAX            std::numeric_limits<int>::min()
#define PRIORITY_RELEASE        -6
#define PRIORITY_MANAGER        -5
#define PRIORITY_RELEASE_POST   -4
#define PRIORITY_SEND           -3
#define PRIORITY_SIGNAL         -2
#define PRIORITY_TRAP           -1
#define PRIORITY_MIN            std::numeric_limits<int>::max()

#define SUCCESS    0
#define ENQUEUE   -1
#define REJECT    -2
#define BLOCK     std::numeric_limits<double>::infinity()

#define COMMA ,

#define BASE_CLONEABLE(Type) virtual Type* clone() const = 0;
#define CLONEABLE(Type) virtual Type* clone() const { return new Type(*this); }

typedef UMAP<std::string, double> Attr;

template <typename T>
std::ostream& operator<<(std::ostream& out, const VEC<T>& v) {
  out << "[";
  if (!v.empty())
    std::copy(v.begin(), v.end(), std::ostream_iterator<T>(out, ", "));
  out << "\b\b]";
  return out;
}

template <typename T, typename U, typename V>
class FnWrap {
public:
  V arg;

  FnWrap(const Fn<T(U)>& call, const V& arg) : arg(arg), call(call) {}

  T operator()(U param) { return call(param); }

  friend std::ostream& operator<<(std::ostream& out, const FnWrap<T, U, V>& fn) {
    out << fn.arg;
    return out;
  }

private:
  Fn<T(U)> call;
};

#endif
