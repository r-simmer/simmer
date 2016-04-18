#ifndef PROCESS_H
#define PROCESS_H

#include "entity.h"

// forward declarations
class Activity;
class Generator;

/** 
 * Abstract class for processes, active entities that need a method run().
 */
class Process: public Entity {
public:
  Process(Simulator* sim, std::string name, int mon, bool generator=false): 
    Entity(sim, name, mon), generator(generator), active(true) {}
  virtual ~Process(){}
  virtual void run() = 0;
  inline virtual void activate() { active = true; }
  inline virtual void deactivate(bool restart) { active = false; }
  inline bool is_generator() { return generator; }
  inline bool is_active() { return active; }
private:
  bool generator;
  bool active;
};

typedef UMAP<std::string, double> Attr;

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
    Process(sim, name, mon), activity(first_activity), gen(gen), busy_until(-1), remaining(0) {}
  
  void run();
  void activate();
  void deactivate(bool restart);
  
  int set_attribute(std::string key, double value);
  inline Attr* get_attributes() { return &attributes; }
  
  inline void set_start(std::string name, double start) { restime[name].start = start; }
  inline double get_start(std::string name) { return restime[name].start; }
  inline double get_start() { return lifetime.start; }
  
  inline void set_activity(std::string name, double act) { restime[name].activity = act; }
  inline double get_activity(std::string name) { return restime[name].activity; }
  inline double get_activity() { return lifetime.activity; }
  
  inline double get_remaining() { return remaining; }
  
  void leave(std::string name, double time);
  void reject(double time);

private:
  ArrTime lifetime;   /**< time spent in the whole trajectory */
  ResTime restime;    /**< time spent in resources */
  Activity* activity; /**< current activity from an R trajectory */
  Generator* gen;     /**< parent generator */
  Attr attributes;    /**< user-defined (key, value) pairs */
  double busy_until;  /**< next scheduled event time */
  double remaining;   /**< time remaining in a deactivated arrival */
};

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
    Process(sim, name_prefix, mon, true), count(0), first_activity(first_activity), dist(dist) {}
  
  /**
   * Reset the generator: counter, statistics.
   */
  void reset() { 
    count = 0;
    traj_stats.clear();
    res_stats.clear();
    attr_stats.clear();
  }
  
  void run();
  
  /**
   * Gather attribute statistics.
   */
  inline void observe(double time, Arrival* arrival, std::string key) {
    Attr* attributes = arrival->get_attributes();
    attr_stats.insert("time",   time);
    attr_stats.insert("name",   arrival->name);
    attr_stats.insert("key",    key);
    attr_stats.insert("value",  (*attributes)[key]);
  }
  
  /**
   * Arrivals notify their end with this call.
   * The generator is in charge of gathering statistics and deleting the arrival.
   * @param   time      ending time
   * @param   arrival   a pointer to the ending arrival
   * @param   finished  bool that indicates whether the arrival has finished its trajectory
   */
  inline void notify_end(double time, Arrival* arrival, bool finished) {
    if (is_monitored() >= 1) {
      traj_stats.insert("name",           arrival->name);
      traj_stats.insert("start_time",     arrival->get_start());
      traj_stats.insert("end_time",       time);
      traj_stats.insert("activity_time",  arrival->get_activity());
      traj_stats.insert("finished",       finished);
    }
    delete arrival;
  }
  
  /**
   * Arrivals notify resource releases with this call.
   * @param   time      ending time
   * @param   arrival   a pointer to the arrival
   * @param   resource  name of the resource released
   */
  inline void notify_release(double time, Arrival* arrival, std::string resource) {
    res_stats.insert("name",          arrival->name);
    res_stats.insert("start_time",    arrival->get_start(resource));
    res_stats.insert("end_time",      time);
    res_stats.insert("activity_time", arrival->get_activity(resource));
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

#endif
