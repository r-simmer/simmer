#ifndef ENTITY_H
#define ENTITY_H

#include <Rcpp.h>
#include <queue>

class Simulator;

class Entity {
public:
  Simulator* sim;
  std::string name;
	Entity(Simulator* sim, std::string name, bool mon): 
	  sim(sim), name(name), mon(mon) {}
	virtual ~Entity(){}
	inline bool is_monitored() { return mon; }
	
private:
  bool mon;
};

class Process: public Entity {
public:
  Process(Simulator* sim, std::string name, bool mon): Entity(sim, name, mon) {}
  virtual ~Process(){}
  virtual void activate() { throw std::runtime_error("method not implemented"); }
  inline virtual bool is_generator() { return 0; }
};

class Arrival: public Process {
public:
  double start_time;
  double activity_time;
  
  Arrival(Simulator* sim, std::string name, bool mon, Rcpp::Environment first_activity):
    Process(sim, name, mon), start_time(-1), activity_time(0), activity(first_activity) {}
  
  void activate();
  
private:
  Rcpp::Environment activity;
};

class Generator: public Process {
public:
  Generator(Simulator* sim, std::string name_prefix, bool mon,
            Rcpp::Environment first_activity, Rcpp::Function dist): 
    Process(sim, name_prefix, mon), count(0), first_activity(first_activity), dist(dist) {}
  
  void activate();
  inline bool is_generator() { return 1; }
  
private:
  int count;
  Rcpp::Environment first_activity;
  Rcpp::Function dist;
};

class ResStats {
public:
  std::vector<double> time;
  std::vector<double> server;
  std::vector<double> queue;
};

class Resource: public Entity {
public:
  Resource(Simulator* sim, std::string name, bool mon, int capacity, int queue_size): 
    Entity(sim, name, mon), server_count(0), capacity(capacity), queue_count(0), 
    queue_size(queue_size) {
    res_stats = new ResStats();
  }
  
  ~Resource() {
    while (!queue.empty()) {
      delete queue.front().first;
      queue.pop();
    }
    delete res_stats;
  }
  
  void reset() {
    server_count = 0;
    queue_count = 0;
    while (!queue.empty()) {
      delete queue.front().first;
      queue.pop();
    }
    delete res_stats;
    res_stats = new ResStats();
  }
  
  int seize(Arrival* arrival, int amount);
  int release(Arrival* arrival, int amount);
  
  void observe(double time) {
    res_stats->time.push_back(time);
    res_stats->server.push_back(server_count);
    res_stats->queue.push_back(queue_count);
  }
  
  ResStats* get_observations() { return res_stats; }
  int get_capacity() { return capacity; }
  int get_queue_size() { return queue_size; }
  
private:
  int server_count;
  int capacity;
  std::queue<std::pair<Arrival*, int> > queue;
  int queue_count;
  int queue_size;
  ResStats* res_stats;
  
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
