#ifndef SIMULATOR_H
#define SIMULATOR_H

#include <Rcpp.h>

#include <map>
#include <limits>
#include <algorithm>
#include <list>
#include <queue>

#include "entity.h"

class Event {
public:
  double time;
  Process* process;
  Event(double time, Process* process): time(time), process(process) {}
  ~Event() { delete process; }
};

struct EventOrder {
  bool operator()(const Event* lhs, const Event* rhs) const {
    return lhs->time < rhs->time;
  }
};

typedef std::priority_queue<Event*, std::vector<Event*>, EventOrder> PQueue;
typedef std::map<std::string, Resource*> ResMap;
typedef std::vector<Generator*> GenVec;

class ArrStats {
public:
  std::vector<std::string> name;
  std::vector<double> start_time;
  std::vector<double> end_time;
  std::vector<double> activity_time;
  std::vector<bool> finished;
};

class Simulator {
public:
  std::string name;
  bool verbose;
  
  Simulator(std::string name, bool verbose): 
    name(name), verbose(verbose), now_(0) {
    arrival_stats = new ArrStats();
  }
  
  ~Simulator() {
    while (!event_queue.empty()) {
      delete event_queue.top();
      event_queue.pop();
    }
    resource_map.clear();
    generator_vec.clear();
    delete arrival_stats;
  }
  
  void reset() {
    now_ = 0;
    while (!event_queue.empty()) {
      delete event_queue.top();
      event_queue.pop();
    }
    for (ResMap::iterator itr = resource_map.begin(); itr != resource_map.end(); ++itr)
      itr->second->reset();
    delete arrival_stats;
    arrival_stats = new ArrStats();
  }
  
  inline double now() { return now_; }
  
  inline void schedule(double delay, Process* process) {
    Event* ev = new Event(now_ + delay, process);
    event_queue.push(ev);
  }
  
  inline void notify_end(Arrival* arrival, bool finished) {
    arrival_stats->name.push_back(arrival->name);
    arrival_stats->start_time.push_back(arrival->start_time);
    arrival_stats->end_time.push_back(now_);
    arrival_stats->activity_time.push_back(arrival->activity_time);
    arrival_stats->finished.push_back(finished);
    delete arrival;
  }
  
  void run(double until) {
    // Initialize generators if the queue is empty
    if (event_queue.empty()) {
      if (generator_vec.empty())
        throw std::runtime_error("no generators defined");
      for (GenVec::iterator itr = generator_vec.begin(); itr != generator_vec.end(); ++itr)
        itr->activate();
    }
    // Loop
    while(now_ < until) {
      Event* ev = get_next();
      now_ = ev->time;
      ev->process->activate();
    }
  }
  
  void add_generator(std::string name_prefix, 
                     Rcpp::Environment* first_event, Rcpp::Function* dist) {
    Generator* gen = new Generator(this, name_prefix, first_event, dist);
    generator_vec.push_back(gen);
  }
  
  void add_resource(std::string name, int capacity, int queue_size, bool mon) {
    Resource* res = new Resource(this, name, capacity, queue_size, mon);
    resource_map[name] = res;
  }
  
  Resource* get_resource(std::string name) {
    try {
      return resource_map[name];
    } catch (...) {
      // not found
      throw std::runtime_error("resource '" + name + "' not found (typo?)");
    }
  }
  
  ArrStats* get_mon_arrivals() { return arrival_stats; }
  
  ResStats* get_mon_resource(std::string name) { 
    return get_resource(name)->get_observations();
  }
  
private:
  double now_;
  PQueue event_queue;
  ResMap resource_map;
  GenVec generator_vec;
  ArrStats* arrival_stats;
  
  inline Event* get_next() {
    Event* ev = event_queue.top();
    event_queue.pop();
    return ev;
  }
};

#endif
