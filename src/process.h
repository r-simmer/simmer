#ifndef PROCESS_H
#define PROCESS_H

#include "entity.h"

// forward declarations
class Activity;
class Arrival;

/** 
 * Abstract class for processes, active entities that need a method run().
 */
class Process: public Entity {
public:
  Process(Simulator* sim, std::string name, int mon, bool is_arrival=false): 
    Entity(sim, name, mon), is_arrival_(is_arrival), is_active_(true) {}
  virtual ~Process(){}
  virtual void run() = 0;
  virtual void activate() { is_active_ = true; }
  virtual void deactivate(bool restart) { is_active_ = false; }
  bool is_arrival() { return is_arrival_; }
  bool is_active() { return is_active_; }
private:
  bool is_arrival_;
  bool is_active_;
};

template <typename T>
class Manager: public Process {
  typedef boost::function<void (T)> Setter;
  
public:
  Manager(Simulator* sim, std::string name, VEC<double> duration, VEC<T> value, Setter set):
    Process(sim, name, false), duration(duration), value(value), set(set), index(0) {}
  
  void run();
  
private:
  VEC<double> duration;
  VEC<T> value;
  Setter set;
  unsigned int index;
};

typedef UMAP<std::string, double> Attr;

/**
 * Generation process.
 */
class Generator: public Process {
public:
  /**
   * Constructor.
   * @param sim             a pointer to the simulator
   * @param name            the name
   * @param mon             int that indicates whether this entity must be monitored
   * @param first_activity  the first activity of a user-defined R trajectory
   * @param dist            an user-defined R function that provides random numbers
   */
  Generator(Simulator* sim, std::string name_prefix, int mon,
            Activity* first_activity, Rcpp::Function dist): 
    Process(sim, name_prefix, mon), count(0), first_activity(first_activity), dist(dist) {}
  
  /**
   * Reset the generator: counter, statistics.
   */
  virtual void reset() { 
    count = 0;
    traj_stats.clear();
    res_stats.clear();
    attr_stats.clear();
  }
  
  void run();
  
  /**
   * Gather attribute statistics.
   */
  void observe(double time, std::string name, std::string key, double value) {
    attr_stats.insert("time",   time);
    attr_stats.insert("name",   name);
    attr_stats.insert("key",    key);
    attr_stats.insert("value",  value);
  }
  
  /**
   * Arrivals notify their end with this call.
   * The generator is in charge of gathering statistics and deleting the arrival.
   */
  void notify_end(std::string name, double start, double end, 
                         double activity, bool finished) {
    traj_stats.insert("name",           name);
    traj_stats.insert("start_time",     start);
    traj_stats.insert("end_time",       end);
    traj_stats.insert("activity_time",  activity);
    traj_stats.insert("finished",       finished);
  }
  
  /**
   * Arrivals notify resource releases with this call.
   */
  void notify_release(std::string name, double start, double end, 
                             double activity, std::string resource) {
    res_stats.insert("name",          name);
    res_stats.insert("start_time",    start);
    res_stats.insert("end_time",      end);
    res_stats.insert("activity_time", activity);
    res_stats.insert("resource",      resource);
  }
  
  /**
   * Get the monitoring data.
   */
  StatsMap* get_traj_observations() { return &traj_stats; }
  StatsMap* get_res_observations() { return &res_stats; }
  StatsMap* get_attr_observations() { return &attr_stats; }
  int get_n_generated() { return count; }
  
private:
  int count;                /**< number of arrivals generated */
  Activity* first_activity;
  Rcpp::Function dist;
  StatsMap traj_stats;      /**< arrival statistics per trajectory */
  StatsMap res_stats;       /**< arrival statistics per resource */
  StatsMap attr_stats;      /**< attribute statistics */
};

/** 
 *  Arrival process.
 */
class Arrival: public Process {
  struct ArrTime {
    double start;
    double activity;
    ArrTime(): start(-1), activity(0) {}
  };
  typedef UMAP<std::string, ArrTime> ResTime;
  
public:
  /**
  * Constructor.
  * @param sim             a pointer to the simulator
  * @param name            the name
  * @param mon             int that indicates whether this entity must be monitored
  * @param first_activity  the first activity of a user-defined R trajectory
  */
  Arrival(Simulator* sim, std::string name, int mon, Activity* first_activity, Generator* gen):
    Process(sim, name, mon, true), activity(first_activity), gen(gen), busy_until(-1), remaining(0) {}
  
  void run();
  void activate();
  void deactivate(bool restart);
  
  int set_attribute(std::string key, double value);
  Attr* get_attributes() { return &attributes; }
  double get_remaining() { return remaining; }
  
  void set_start(std::string name, double start) { restime[name].start = start; }
  void set_activity(std::string name, double act) { restime[name].activity = act; }
  double get_activity(std::string name) { return restime[name].activity; }
  
  void leave(std::string resource, double time) {
    gen->notify_release(name, restime[resource].start, time, 
                        restime[resource].activity, resource);
  }
  
  void terminate(double time, bool finished) {
    if (is_monitored() >= 1)
      gen->notify_end(name, lifetime.start, time, lifetime.activity, finished);
    delete this;
  }
  
private:
  ArrTime lifetime;   /**< time spent in the whole trajectory */
  ResTime restime;    /**< time spent in resources */
  Activity* activity; /**< current activity from an R trajectory */
  Generator* gen;     /**< parent generator */
  Attr attributes;    /**< user-defined (key, value) pairs */
  double busy_until;  /**< next scheduled event time */
  double remaining;   /**< time remaining in a deactivated arrival */
};

#endif
