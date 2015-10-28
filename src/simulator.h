#ifndef SIMULATOR_H
#define SIMULATOR_H

#include <Rcpp.h>
#include <queue>
#include <map>

#include "entity.h"

/**
 * Event container. Encapsulates future processes in the event queue.
 */
class Event {
public:
  double time;
  Process* process;
  
  /**
   * Constructor.
   * @param time    time of occurrence in the future
   * @param process a pointer to the process to activate
   */
  Event(double time, Process* process): time(time), process(process) {}
};

/**
 * Custom event ordering operator for the event queue (earlier events first).
 * I don't really understand this operator, but it works this way. :-P
 */
struct EventOrder {
  bool operator()(const Event* lhs, const Event* rhs) const {
    return lhs->time > rhs->time;
  }
};

typedef std::priority_queue<Event*, std::vector<Event*>, EventOrder> PQueue;
typedef std::map<std::string, Resource*> ResMap;
typedef std::vector<Generator*> GenVec;

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
};

/**
 * The simulator.
 */
class Simulator {
public:
  int n;
  bool verbose;
  
  /**
   * Constructor.
   * @param n       simulator identifier
   * @param verbose verbose flag
   */
  Simulator(int n, bool verbose): 
    n(n), verbose(verbose), now_(0) {
    arrival_stats = new ArrStats();
  }
  
  ~Simulator() {
    while (!event_queue.empty()) {
      if (!event_queue.top()->process->is_generator())
        delete event_queue.top()->process;
      delete event_queue.top();
      event_queue.pop();
    }
    resource_map.clear();
    generator_vec.clear();
    delete arrival_stats;
  }
  
  /**
   * Reset the simulation: time, event queue, resources, generators and statistics.
   */
  void reset() {
    now_ = 0;
    while (!event_queue.empty()) {
      if (!event_queue.top()->process->is_generator())
        delete event_queue.top()->process;
      delete event_queue.top();
      event_queue.pop();
    }
    for (ResMap::iterator itr = resource_map.begin(); itr != resource_map.end(); ++itr)
      itr->second->reset();
    for (GenVec::iterator itr = generator_vec.begin(); itr != generator_vec.end(); ++itr)
      (*itr)->reset();
    delete arrival_stats;
    arrival_stats = new ArrStats();
  }
  
  inline double now() { return now_; }
  
  /**
   * Schedule a future event.
   * @param   delay   delay from now()
   * @param   process the process to schedule
   */
  inline void schedule(double delay, Process* process) {
    Event* ev = new Event(now_ + delay, process);
    event_queue.push(ev);
  }
  
  /**
   * Entities notify the end of an arrival with this call.
   * The simulator is in charge of gathering statistics and deleting the arrival.
   * @param   arrival   a pointer to the ending arrival
   * @param   finished  bool that indicates whether the arrival has finished its trajectory
   */
  void notify_end(Arrival* arrival, bool finished);
  
  /**
   * Run the simulation.
   * @param   until   time of ending
   */
  void run(double until);
  
  /**
   * Add a generator of arrivals to the simulator.
   * @param   name_prefix     prefix for the arrival names
   * @param   first_activity  the first activity of a user-defined R trajectory
   * @param   dis             an user-defined R function that provides random numbers
   * @param   mon             bool that indicates whether this entity must be monitored
   */
  void add_generator(std::string name_prefix, 
                     Rcpp::Environment first_activity, Rcpp::Function dist, bool mon) {
    Generator* gen = new Generator(this, name_prefix, mon, first_activity, dist);
    generator_vec.push_back(gen);
  }
  
  /**
   * Add a resource to the simulator.
   * @param   name        the name
   * @param   capacity    server capacity (-1 means infinity)
   * @param   queue_size  room in the queue (-1 means infinity)
   * @param   mon         bool that indicates whether this entity must be monitored
   */
  void add_resource(std::string name, int capacity, int queue_size, bool mon) {
    Resource* res = new Resource(this, name, mon, capacity, queue_size);
    resource_map[name] = res;
  }
  
  /**
   * Get a resource by name.
   */
  Resource* get_resource(std::string name) {
    try {
      return resource_map[name];
    } catch (...) {
      // not found
      throw std::runtime_error("resource '" + name + "' not found (typo?)");
    }
  }
  
  /**
   * Get the monitoring data from the arrivals.
   */
  ArrStats* get_mon_arrivals() { return arrival_stats; }
  
  /**
   * Get the monitoring data from a resource by name.
   */
  ResStats* get_mon_resource(std::string name) { 
    return get_resource(name)->get_observations();
  }
  
private:
  double now_;              /**< simulation time */
  PQueue event_queue;       /**< the event queue */
  ResMap resource_map;      /**< map of resources */
  GenVec generator_vec;     /**< vector of generators */
  ArrStats* arrival_stats;  /**< arrival statistics */
  
  inline Event* get_next() {
    Event* ev = event_queue.top();
    event_queue.pop();
    return ev;
  }
};

#endif
