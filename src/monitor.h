#ifndef MONITOR_H
#define MONITOR_H

#include "simmer.h"

class Monitor {
  typedef boost::variant< VEC<bool>, VEC<int>, VEC<double>, VEC<std::string> > MonVec;
  typedef UMAP<std::string, MonVec> MonMap;

public:
  template <typename T>
  VEC<T> get(const std::string& key) const {
    MonMap::const_iterator search = map.find(key);
    if (search != map.end())
      return boost::get< VEC<T> >(search->second);
    return VEC<T>();
  }

  template <typename T>
  void insert(const std::string& key, const T& value) {
    if (map.find(key) == map.end())
      map.insert(std::pair< std::string, VEC<T> >(key, VEC<T>()));
    boost::get< VEC<T> >(map[key]).push_back(value);
  }

  void clear() { map.clear(); }

private:
  MonMap map;
};

#endif
