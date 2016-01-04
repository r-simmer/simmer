#ifndef STATS_H
#define STATS_H

#include "simmer.h"
#include <boost/variant.hpp>

class StatsMap {
  typedef boost::variant< VEC<bool>, VEC<int>, VEC<double>, VEC<std::string> > StatsVec;
  typedef MAP<std::string, StatsVec> StatsContainer;
  
  class clear_vec: public boost::static_visitor<> {
  public:
    template <typename T>
    void operator()(T & operand) const { operand.clear(); }
  };
  
public:
  template <typename T>
  inline VEC<T> get(std::string key) {
    if (map.find(key) != map.end())
      return boost::get< VEC<T> >(map[key]);
    return VEC<T>();
  }
  
  template <typename T>
  inline void insert(std::string key, T value) {
    if (map.find(key) == map.end())
      map.insert(std::pair< std::string, VEC<T> >(key, VEC<T>()));
    boost::get< VEC<T> >(map[key]).push_back(value);
  }
  
  void clear() { 
    foreach_ (StatsContainer::value_type& itr, map)
      boost::apply_visitor(clear_vec(), itr.second);
  }
  
private:
  StatsContainer map;
};

#endif
