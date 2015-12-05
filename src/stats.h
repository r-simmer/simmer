#ifndef STATS_H
#define STATS_H

/**
* Arrival statistics.
*/
class ArrStats {
public:
  VEC<std::string> name;
  VEC<double> start_time;
  VEC<double> end_time;
  VEC<double> activity_time;
  VEC<bool> finished;
  
  void clear() {
    name.clear();
    start_time.clear();
    end_time.clear();
    activity_time.clear();
    finished.clear();
  }
};

/**
 * Attribute statistics.
 */
class AttrStats {
public:
  VEC<double> time;
  VEC<std::string> name;
  VEC<std::string> key;
  VEC<double> value;
  
  void clear() {
    time.clear();
    name.clear();
    key.clear();
    value.clear();
  }
};

/**
 * Resource statistics.
 */
class ResStats {
public:
  VEC<double> time;
  VEC<double> server;
  VEC<double> queue;
  
  void clear() {
    time.clear();
    server.clear();
    queue.clear();
  }
};

#endif
