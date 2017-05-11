#ifndef STATS_H
#define STATS_H

#include "simmer.h"

class StatsMap {
  typedef boost::variant< VEC<bool>, VEC<int>, VEC<double>, VEC<std::string> > StatsVec;
  typedef UMAP<std::string, StatsVec> StatsContainer;

public:
  template <typename T>
  VEC<T> get(std::string key) {
    if (map.find(key) != map.end())
      return boost::get< VEC<T> >(map[key]);
    return VEC<T>();
  }

  template <typename T>
  void insert(std::string key, T value) {
    if (map.find(key) == map.end())
      map.insert(std::pair< std::string, VEC<T> >(key, VEC<T>()));
    boost::get< VEC<T> >(map[key]).push_back(value);
  }

  void clear() { map.clear(); }

private:
  StatsContainer map;
};

#endif
