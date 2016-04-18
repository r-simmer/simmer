#ifndef SIMMER_H
#define SIMMER_H

// [[Rcpp::depends(BH)]]
#include <Rcpp.h>

#include <queue>
#include <boost/heap/priority_queue.hpp>
#include <boost/container/set.hpp>
#include <boost/unordered_set.hpp>
#include <boost/unordered_map.hpp>

#define VEC std::vector
#define QUEUE std::queue
#define PQUEUE boost::heap::priority_queue
#define MSET boost::container::multiset
#define USET boost::unordered_set
#define UMAP boost::unordered_map

#include <boost/foreach.hpp>

#define foreach_    BOOST_FOREACH
#define foreach_r_  BOOST_REVERSE_FOREACH

#include <boost/function.hpp>
#include <boost/bind.hpp>

#endif
