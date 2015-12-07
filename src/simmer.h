#ifndef SIMMER_H
#define SIMMER_H

// [[Rcpp::depends(BH)]]
#include <Rcpp.h>
#include <queue>
#include <boost/heap/priority_queue.hpp>
#include <boost/unordered_set.hpp>
#include <boost/unordered_map.hpp>

#define VEC std::vector
#define QUEUE std::queue
#define PQUEUE boost::heap::priority_queue
#define SET boost::unordered_set
#define MAP boost::unordered_map

#include <boost/foreach.hpp>

#define foreach_ BOOST_FOREACH

#endif
