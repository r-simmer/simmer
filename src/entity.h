#ifndef ENTITY_H
#define ENTITY_H

#include <Rcpp.h>
#include <queue>

class Simulator;

class Entity {
public:
  Simulator* sim;
  std::string name;
	Entity(Simulator* sim, std::string name): sim(sim), name(name) {}
	virtual ~Entity();
};

class Process: public Entity {
public:
  Process(Simulator* sim, std::string name): Entity(sim, name) {}
  virtual ~Process();
  virtual void activate() { throw std::runtime_error("method not implemented"); }
};

class Arrival: public Process {
public:
  double start_time;
  double activity_time;
  
  Arrival(Simulator* sim, std::string name, Rcpp::Environment first_event):
    Process(sim, name), start_time(-1), activity_time(0), event(first_event) {}
  
  void activate();
  
private:
  Rcpp::Environment event;
};

class Generator: public Process {
public:
  Generator(Simulator* sim, std::string name_prefix, 
            Rcpp::Environment first_event, Rcpp::Function dist): 
    Process(sim, name_prefix), count(0), first_event(first_event), dist(dist) {}
  
  void activate();
  
private:
  int count;
  Rcpp::Environment first_event;
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
  Resource(Simulator* sim, std::string name, int capacity, int queue_size, bool mon): 
    Entity(sim, name), server_count(0), capacity(capacity), queue_count(0), 
    queue_size(queue_size), mon(mon) {
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
  bool mon;
  ResStats* res_stats;
  
  inline bool room_in_server(int amount) { return server_count + amount < capacity; }
  inline bool room_in_queue(int amount) { return queue_count + amount < queue_size; }
};

#endif
