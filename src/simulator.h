#ifndef SIMULATOR_H
#define SIMULATOR_H

#include <Rcpp.h>
#include <queue>
#include <map>

#include "entity.h"

// forward declarations
class Activity;

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
typedef std::map<std::string, Entity*> EntMap;

/**
 * The simulator.
 */
class Simulator {
public:
  std::string name;
  bool verbose;
  
  /**
   * Constructor.
   * @param name    simulator name
   * @param verbose verbose flag
   */
  Simulator(std::string name, bool verbose): name(name), verbose(verbose), now_(0) {}
  
  ~Simulator() {
    while (!event_queue.empty()) {
      if (!event_queue.top()->process->is_generator())
        delete event_queue.top()->process;
      delete event_queue.top();
      event_queue.pop();
    }
    resource_map.clear();
    generator_map.clear();
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
    for (EntMap::iterator itr = resource_map.begin(); itr != resource_map.end(); ++itr)
      ((Resource*)itr->second)->reset();
    for (EntMap::iterator itr = generator_map.begin(); itr != generator_map.end(); ++itr) {
      ((Generator*)itr->second)->reset();
      ((Generator*)itr->second)->activate();
    }
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
   * Get the time of the next scheduled event.
   */
  double peek() { 
    if (!event_queue.empty())
      return event_queue.top()->time;
    else return -1;
  }
  
  /**
   * Process the next event. Only one step, a giant leap for mankind.
   */
  inline bool step() {
    Event* ev = get_next();
    if (!ev) return 0;
    now_ = ev->time;
    ev->process->activate();
    delete ev;
    return 1;
    // ... and that's it! :D
  }
  
  /**
   * Executes steps until the given criterion is met.
   * @param   until   time of ending
   */
  void run(double until) { while ((now_ < until) && step()); }
  
  /**
   * Add a generator of arrivals to the simulator.
   * @param   name_prefix     prefix for the arrival names
   * @param   first_activity  the first activity of a user-defined R trajectory
   * @param   dis             an user-defined R function that provides random numbers
   * @param   mon             int that indicates whether this entity must be monitored
   */
  bool add_generator(std::string name_prefix, 
                     Activity* first_activity, Rcpp::Function dist, int mon) {
    if (generator_map.find(name_prefix) == generator_map.end()) {
      Generator* gen = new Generator(this, name_prefix, mon, first_activity, dist);
      generator_map[name_prefix] = gen;
      gen->activate();
      return TRUE;
    }
    Rcpp::warning("generator " + name + " already defined");
    return FALSE;
  }
  
  /**
   * Add a resource to the simulator.
   * @param   name        the name
   * @param   capacity    server capacity (-1 means infinity)
   * @param   queue_size  room in the queue (-1 means infinity)
   * @param   mon         bool that indicates whether this entity must be monitored
   */
  bool add_resource(std::string name, int capacity, int queue_size, bool mon) {
    if (resource_map.find(name) == resource_map.end()) {
      Resource* res = new Resource(this, name, mon, capacity, queue_size);
      resource_map[name] = res;
      return TRUE;
    }
    Rcpp::warning("resource " + name + " already defined");
    return FALSE;
  }
  
  /**
   * Get a generator by name.
   */
  Generator* get_generator(std::string name) {
    EntMap::iterator search = generator_map.find(name);
    if (search == generator_map.end())
      Rcpp::stop("generator '" + name + "' not found (typo?)");
    return (Generator*)search->second;
  }
  
  /**
   * Get a resource by name.
   */
  Resource* get_resource(std::string name) {
    EntMap::iterator search = resource_map.find(name);
    if (search == resource_map.end())
      Rcpp::stop("resource '" + name + "' not found (typo?)");
    return (Resource*)search->second;
  }
  
private:
  double now_;              /**< simulation time */
  PQueue event_queue;       /**< the event queue */
  EntMap resource_map;      /**< map of resources */
  EntMap generator_map;     /**< map of generators */
  
  inline Event* get_next() {
    if (event_queue.empty()) return NULL;
    Event* ev = event_queue.top();
    event_queue.pop();
    return ev;
  }
};

#endif
