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

#include <boost/optional.hpp>

#define OPT boost::optional
#define NONE boost::none

#include <boost/foreach.hpp>

#define foreach_    BOOST_FOREACH
#define foreach_r_  BOOST_REVERSE_FOREACH

#include <boost/variant.hpp>
#include <boost/function.hpp>
#include <boost/bind.hpp>

#define FMT(n, justify) std::setw(n) << std::justify

#define PRIORITY_MAX            std::numeric_limits<int>::min()
#define PRIORITY_RELEASE        -4
#define PRIORITY_MANAGER        -3
#define PRIORITY_RELEASE_POST   -2
#define PRIORITY_GENERATOR      -1
#define PRIORITY_MIN            std::numeric_limits<int>::max()

#define BASE_CLONEABLE(Type) \
  virtual Type* clone() const = 0;

#define CLONEABLE(Type) \
  virtual Type* clone() const { return new Type(*this); }
  
#define CLONEABLE_COUNT(Type) \
  virtual Type* clone() const { (*clones)++; return new Type(*this); } \
  int* clones;

#endif
