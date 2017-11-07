#ifndef SIMMER_H
#define SIMMER_H

// [[Rcpp::depends(BH)]]
#include <Rcpp.h>

#include <boost/container/set.hpp>
#include <boost/unordered_set.hpp>
#include <boost/unordered_map.hpp>

#define VEC std::vector

template <typename T>
std::ostream& operator<<(std::ostream& out, const VEC<T>& v) {
  out << "[";
  if (!v.empty())
    std::copy(v.begin(), v.end(), std::ostream_iterator<T>(out, ", "));
  out << "\b\b]";
  return out;
}

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

#define BIND(T) boost::function<T ()>

#define FMT(n, justify) std::setw(n) << std::justify
#define IND(n) std::string(n, ' ')

#define PRIORITY_MAX            std::numeric_limits<int>::min()
#define PRIORITY_RELEASE        -4
#define PRIORITY_MANAGER        -3
#define PRIORITY_RELEASE_POST   -2
#define PRIORITY_GENERATOR      -1
#define PRIORITY_MIN            std::numeric_limits<int>::max()

#define SUCCESS    0
#define ENQUEUE   -1
#define REJECT    -2
#define BLOCK     std::numeric_limits<double>::infinity()

#define COMMA ,

#define BASE_CLONEABLE(Type) \
  virtual Type* clone() const = 0;

#define CLONEABLE(Type) \
  virtual Type* clone() const { return new Type(*this); }

typedef UMAP<std::string, double> Attr;

#endif
