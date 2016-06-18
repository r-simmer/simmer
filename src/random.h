#ifndef RANDOM_H
#define RANDOM_H

#include "simmer.h"

#include <boost/random/mersenne_twister.hpp>
#include <boost/random/discrete_distribution.hpp>

static int rand_int(int min, int max) {
  static boost::mt19937 mersenne_gen;
  boost::random::uniform_int_distribution<> dist(min, max);
  return dist(mersenne_gen);
}

#endif
