#ifndef STATS_H
#define STATS_H

/**
* Arrival statistics.
*/
class ArrStats {
public:
  std::vector<std::string> name;
  std::vector<double> start_time;
  std::vector<double> end_time;
  std::vector<double> activity_time;
  std::vector<bool> finished;
  
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
  std::vector<double> time;
  std::vector<std::string> name;
  std::vector<std::string> key;
  std::vector<double> value;
  
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
  std::vector<double> time;
  std::vector<double> server;
  std::vector<double> queue;
  
  void clear() {
    time.clear();
    server.clear();
    queue.clear();
  }
};

#endif
