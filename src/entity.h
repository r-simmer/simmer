#ifndef ENTITY_H
#define ENTITY_H

#include "simmer.h"
#include "stats.h"

#define SUCCESS 0
#define ENQUEUED -1
#define REJECTED -2

// forward declarations
class Simulator;
class Activity;
class Generator;

/** 
 *  Base class. Every element in a simulation model is an entity.
 */
class Entity {
public:
  Simulator* sim;
  std::string name;
  
  Entity(Simulator* sim, std::string name, int mon): 
    sim(sim), name(name), mon(std::abs(mon)) {}
  virtual ~Entity(){}
  inline int is_monitored() { return mon; }
  
private:
  int mon;
};

/** 
 * Abstract class for processes, active entities that need a method activate().
 */
class Process: public Entity {
public:
  Process(Simulator* sim, std::string name, int mon, bool is_gen=false): 
    Entity(sim, name, mon), is_gen(is_gen) {}
  virtual ~Process(){}
  virtual void activate() = 0;
  inline bool is_generator() { return is_gen; }
private:
  bool is_gen;
};

typedef MAP<std::string, double> Attr;

/** 
 *  Arrival process.
 */
class Arrival: public Process {
  struct ArrTime {
    double start;
    double activity;
    ArrTime(): start(-1), activity(0) {}
  };
  typedef MAP<std::string, ArrTime> ResTime;
  
public:
  /**
   * Constructor.
   * @param sim             a pointer to the simulator
   * @param name            the name
   * @param mon             int that indicates whether this entity must be monitored
   * @param first_activity  the first activity of a user-defined R trajectory
   */
  Arrival(Simulator* sim, std::string name, int mon, Activity* first_activity, Generator* gen):
    Process(sim, name, mon), activity(first_activity), gen(gen) {}
  
  void activate();
  
  int set_attribute(std::string key, double value);
  inline Attr* get_attributes() { return &attributes; }
  
  inline void set_start(double start) { lifetime.start = start; }
  inline void set_start(std::string name, double start) { restime[name].start = start; }
  inline double get_start() { return lifetime.start; }
  inline double get_start(std::string name) { return restime[name].start; }
  
  inline void set_activity(double act) { lifetime.activity = act; }
  inline void set_activity(std::string name, double act) { restime[name].activity = act; }
  inline double get_activity() { return lifetime.activity; }
  inline double get_activity(std::string name) { return restime[name].activity; }
  
  void leaving(std::string name, double time);

private:
  ArrTime lifetime;   /**< time spent in the whole trajectory */
  ResTime restime;    /**< time spent in resources */
  Activity* activity; /**< current activity from an R trajectory */
  Generator* gen;     /**< parent generator */
  Attr attributes;    /**< user-defined (key, value) pairs */
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
  
  void activate();
  
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

struct RQItem {
  Arrival* arrival;
  int amount;
  int priority;
  double arrived_at;
  
  RQItem(Arrival* arrival, int amount, int priority, double arrived_at):
    arrival(arrival), amount(amount), priority(priority), arrived_at(arrived_at) {}
  
  bool operator<(const RQItem& other) const {
    if(priority == other.priority)
      return arrived_at > other.arrived_at;
    return priority < other.priority;
  }
};

/** 
*  Generic resource, a passive entity that comprises server + FIFO queue.
*/
class Resource: public Entity {
  typedef PQUEUE<RQItem> RPQueue;
  
public:
  /**
  * Constructor.
  * @param sim         a pointer to the simulator
  * @param name        the name
  * @param mon         int that indicates whether this entity must be monitored
  * @param capacity    server capacity (-1 means infinity)
  * @param queue_size  room in the queue (-1 means infinity)
  */
  Resource(Simulator* sim, std::string name, int mon, int capacity, int queue_size): 
    Entity(sim, name, mon), capacity(capacity), queue_size(queue_size), server_count(0), 
    queue_count(0) {}
  
  ~Resource() { reset(); }
  
  /**
  * Reset the resource: server, queue and statistics.
  */
  void reset() {
    server_count = 0;
    queue_count = 0;
    while (!queue.empty()) {
      delete queue.top().arrival;
      queue.pop();
    }
    res_stats.clear();
  }
  
  /**
  * Seize resources.
  * @param   arrival  a pointer to the arrival trying to seize resources
  * @param   amount   the amount of resources needed
  * @param   priority resource accessing priority
  * 
  * @return  SUCCESS, ENQUEUED, REJECTED
  */
  int seize(Arrival* arrival, int amount, int priority);
  
  /**
  * Release resources.
  * @param   arrival a pointer to the arrival that releases resources
  * @param   amount  the amount of resources released
  * 
  * @return  SUCCESS
  */
  int release(Arrival* arrival, int amount);
  
  /**
  * Gather resource statistics.
  */
  inline void observe(double time) {
    res_stats.insert("time",    time);
    res_stats.insert("server",  server_count);
    res_stats.insert("queue",   queue_count);
  }
  
  StatsMap* get_observations() { return &res_stats; }
  int get_capacity() { return capacity; }
  int get_queue_size() { return queue_size; }
  int get_server_count() { return server_count; }
  int get_queue_count() { return queue_count; }
  
private:
  int capacity;
  int queue_size;
  int server_count;     /**< number of arrivals being served */
  int queue_count;      /**< number of arrivals waiting */
  RPQueue queue;        /**< queue container */
  StatsMap res_stats;   /**< resource statistics */
  
  inline bool room_in_server(int amount) { 
    if (capacity < 0) return 1;
    return server_count + amount <= capacity; 
  }
  inline bool room_in_queue(int amount) { 
    if (queue_size < 0) return 1;
    return queue_count + amount <= queue_size;
  }
};

#endif
