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
    sim(sim), name(name), mon(fabs(mon)) {}
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
  Process(Simulator* sim, std::string name, int mon): Entity(sim, name, mon) {}
  virtual ~Process(){}
  virtual void activate() = 0;
  inline virtual bool is_generator() { return 0; }
};

typedef MAP<std::string, double> Attr;

/** 
 *  Arrival process.
 */
class Arrival: public Process {
public:
  double start_time;    /**< generation time */
double activity_time; /**< time spent doing something in the system (not waiting in a queue) */

/**
 * Constructor.
 * @param sim             a pointer to the simulator
 * @param name            the name
 * @param mon             int that indicates whether this entity must be monitored
 * @param first_activity  the first activity of a user-defined R trajectory
 */
Arrival(Simulator* sim, std::string name, int mon, Activity* first_activity, Generator* gen):
  Process(sim, name, mon), start_time(-1), activity_time(0), activity(first_activity), gen(gen) {}


~Arrival() { attributes.clear(); }

void activate();

int set_attribute(std::string key, double value);
inline Attr* get_attributes() { return &attributes; }

private:
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
  Process(sim, name_prefix, mon), count(0), first_activity(first_activity), dist(dist) {}
  
  ~Generator() { reset(); }
  
  /**
   * Reset the generator: counter, statistics.
   */
  void reset() { 
    count = 0;
    attr_stats.clear();
    arr_stats.clear();
  }
  
  void activate();
  inline bool is_generator() { return 1; }
  
  /**
   * Gather attribute statistics.
   */
  inline void observe(double time, Arrival* arrival, std::string key) {
    Attr* attributes = arrival->get_attributes();
    attr_stats.time.push_back(time);
    attr_stats.name.push_back(arrival->name);
    attr_stats.key.push_back(key);
    attr_stats.value.push_back((*attributes)[key]);
  }
  
  /**
   * Arrivals notify their end with this call.
   * The generator is in charge of gathering statistics and deleting the arrival.
   * @param   arrival   a pointer to the ending arrival
   * @param   finished  bool that indicates whether the arrival has finished its trajectory
   */
  inline void notify_end(double time, Arrival* arrival, bool finished) {
    if (is_monitored() >= 1) {
      arr_stats.name.push_back(arrival->name);
      arr_stats.start_time.push_back(arrival->start_time);
      arr_stats.end_time.push_back(time);
      arr_stats.activity_time.push_back(arrival->activity_time);
      arr_stats.finished.push_back(finished);
    }
    delete arrival;
  }
  
  /**
   * Get the monitoring data.
   */
  ArrStats* get_arr_observations() { return &arr_stats; }
  AttrStats* get_attr_observations() { return &attr_stats; }
  int get_n_generated() { return count; }
  
private:
  int count;  /**< number of arrivals generated */
  Activity* first_activity;
  Rcpp::Function dist;
  ArrStats arr_stats;   /**< arrival statistics */
  AttrStats attr_stats;     /**< attribute statistics */
};

struct RQItem{
  Arrival* arrival;
  int amount;
  int priority;
  double arrived_at;
  
  bool operator<(const RQItem& other) const {
    if(priority == other.priority){
      return arrived_at > other.arrived_at;
    } else {
      return priority < other.priority;    
    }
  }
};


typedef PQUEUE<RQItem> RPQueue;

/** 
*  Generic resource, a passive entity that comprises server + FIFO queue.
*/
class Resource: public Entity {
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
  * @param   arrival a pointer to the arrival trying to seize resources
  * @param   amount  the amount of resources needed
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
    res_stats.time.push_back(time);
    res_stats.server.push_back(server_count);
    res_stats.queue.push_back(queue_count);
  }
  
  ResStats* get_observations() { return &res_stats; }
  int get_capacity() { return capacity; }
  int get_queue_size() { return queue_size; }
  int get_server_count() { return server_count; }
  int get_queue_count() { return queue_count; }
  
private:
  int capacity;
  int queue_size;
  int server_count;     /**< number of arrivals being served */
  int queue_count;      /**< number of arrivals waiting */
  RPQueue queue;          /**< queue container */
  
  ResStats res_stats;   /**< resource statistics */
  
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
