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
   * @param process the process to activate
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
  std::string name;
  bool verbose;
  Process* active_process;
  
  /**
   * Constructor.
   * @param name    simulator name
   * @param verbose verbose flag
   */
  Simulator(std::string name, bool verbose): 
    name(name), verbose(verbose), active_process(NULL), now_(0) {
    arrival_stats = new ArrStats();
  }
  
  ~Simulator() {
    while (!event_queue.empty()) {
      delete event_queue.top()->process;
      delete event_queue.top();
      event_queue.pop();
    }
    resource_map.clear();
    delete arrival_stats;
  }
  
  /**
   * Reset the simulation: time, event queue, resources and statistics.
   */
  void reset() {
    now_ = 0;
    while (!event_queue.empty()) {
      delete event_queue.top()->process;
      delete event_queue.top();
      event_queue.pop();
    }
    for (ResMap::iterator itr = resource_map.begin(); itr != resource_map.end(); ++itr)
      itr->second->reset();
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
   * Insert a new process now.
   */
  void process(Rcpp::Function func) {
    if (verbose)
      Rcpp::Rcout << now_ << ": " << active_process << ": new process" << std::endl;
    
    Process* proc = new Process(this, func, FALSE);
    schedule(0, proc);
  }
  
  /**
   * Insert the active process with a delay.
   */
  void timeout(double delay) { 
    if (verbose)
      Rcpp::Rcout << now_ << ": " << active_process << ": new timeout" << std::endl;
    
    schedule(delay, active_process);
  }
  
  /**
   * Run the simulation. Only one step, a giant leap for mankind.
   */
  inline void step() {
    Event* ev = get_next();
    now_ = ev->time;
    active_process = ev->process;
    delete ev;
    int ret = Rcpp::as<int>(active_process->callback());
    if (!ret) {
      if (verbose)
        Rcpp::Rcout << now_ << ": " << active_process << ": process terminated " << std::endl;
      
      active_process->resource->release();
      delete active_process;
    }
    // ... and that's it! :D
  }
  
  /**
   * Run the simulation.
   * @param   until   time of ending
   */
  void run(double until) { while(now_ < until) step(); }
  
  /**
   * Add a resource to the simulator.
   * @param   name        the name
   * @param   capacity    server capacity (-1 means infinity)
   * @param   queue_size  room in the queue (-1 means infinity)
   * @param   mon         bool that indicates whether this entity must be monitored
   */
  void resource(std::string name, int capacity, int queue_size, bool mon) {
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
  
private:
  double now_;              /**< simulation time */
  PQueue event_queue;       /**< the event queue */
  ResMap resource_map;      /**< map of resources */
  ArrStats* arrival_stats;  /**< arrival statistics */
  
  inline Event* get_next() {
    Event* ev = event_queue.top();
    event_queue.pop();
    return ev;
  }
};

#endif
