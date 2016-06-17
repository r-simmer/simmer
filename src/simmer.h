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

#include <boost/foreach.hpp>

#define foreach_    BOOST_FOREACH
#define foreach_r_  BOOST_REVERSE_FOREACH

#include <boost/variant.hpp>
#include <boost/function.hpp>
#include <boost/bind.hpp>

#define FMT(n, justify) std::setw(n) << std::justify

#define PRIORITY_RELEASE        -4
#define PRIORITY_MANAGER        -3
#define PRIORITY_RELEASE_POST   -2
#define PRIORITY_GENERATOR      -1

#define BASE_CLONEABLE(Type) \
  virtual Type* clone() const = 0;

#define CLONEABLE(Type) \
  virtual Type* clone() const { return new Type(*this); }

#include <boost/random/mersenne_twister.hpp>
#include <boost/random/discrete_distribution.hpp>

static int rand_int(int min, int max) {
  static boost::mt19937 mersenne_gen;
  boost::random::uniform_int_distribution<> dist(min, max);
  return dist(mersenne_gen);
}

#endif
