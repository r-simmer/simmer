#ifndef MONITOR_H
#define MONITOR_H

#include "simmer.h"

class Monitor {
  typedef boost::variant< VEC<bool>, VEC<int>, VEC<double>, VEC<std::string> > MonVec;
  typedef UMAP<std::string, MonVec> MonContainer;

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
  MonContainer map;
};

#endif
