#ifndef ENTITY_H
#define ENTITY_H

#include <memory>

class Simulator;

class Entity {
public:
  std::string name;
  Simulator* sim;
	Entity(Simulator* sim, std::string name): sim(sim), name(name) {}
};

class Process: public Entity {
public:
  Process(Simulator* sim, std::string name): Entity(sim, name) {}
  virtual inline void activate() { throw std::runtime_error("method not implemented"); }
};

class ResStats {
public:
  std::vector<double> time;
  std::vector<double> server;
  std::vector<double> queue;
};

class Arrival: public Process {
public:
  double start_time;
  double activity_time;
  
  Arrival(Simulator* sim, std::string name, Rcpp::Environment* first_event):
    Process(sim, name), event(first_event), start_time(-1), activity_time(0) {}
  
  inline void activate() {
    if (start_time < 0) start_time = sim->now();
    
    Rcpp::Environment* current_event = event;
    if (current_event == NULL)
      sim->notify_end(this, 1);
    else {
      if (sim->verbose)
        Rcpp::Rcout <<
          "rep: " << sim->name << " | " << "time: " << sim->now() << " | " <<
            "arrival: " << name << " | " << "event: " << current_event["name"] <<
              std::endl;
      event = event["next_event"];
      activity_time += current_event["run"](this);
    }
  }
  
private:
  Rcpp::Environment* event;
};

class Generator: public Process {
public:
  Generator(Simulator* sim, std::string name_prefix, 
            Rcpp::Environment* first_event, Rcpp::Function* dist): 
    Process(sim, name_prefix), first_event(first_event), dist(dist), count(0) {}
  
  inline void activate() {
    double delay = dist();
    char numstr[21];
    sprintf(numstr, "%d", count);
    Arrival* arrival = new Arrival(sim, name + numstr, first_event);
    sim->schedule(delay, this);
    sim->schedule(delay, arrival);
    count++;
  }
  
private:
  int count;
  Rcpp::Environment* first_event;
  Rcpp::Function* dist;
}

class Resource: public Entity {
public:
  Resource(Simulator* sim, std::string name, int capacity, int queue_size, bool mon): 
  Entity(sim, name), capacity(capacity), queue_size(queue_size), mon(mon) {
    server_count = 0;
    queue_count = 0;
    res_stats = new ResStats();
  }
  
  ~Resource() {
    while (!queue.empty()) {
      delete queue.top();
      queue.pop();
    }
    delete res_stats;
  }
  
  void reset() {
    server_count = 0;
    queue_count = 0;
    while (!queue.empty()) {
      delete queue.top();
      queue.pop();
    }
    delete res_stats;
    res_stats = new ResStats();
  }
  
  inline int seize(Arrival* arrival, int amount) {
    if (mon) observe(sim->now());
    
    if (room_in_server(amount)) {
      server_count += amount;
      return 0; // serving now
    } else if (room_in_queue(amount)) {
      queue_count += amount;
      queue.push(std::make_pair(arrival, amount));
      return 1; // enqueued
    } else {
      sim->notify_end(arrival, 0);
      return -1; // reject
    }
  }
  
  inline int release(Arrival* arrival, int amount) {
    if (mon) observe(sim->now());
    
    // departure
    server_count -= amount;
    
    // serve from the queue
    if (queue_count) {
      Arrival* another_arrival = queue.front()->first;
      int another_amount = queue.front()->second;
      queue_count -= another_amount;
      server_count += another_amount;
      sim->schedule(0, another_arrival);
    }
    return 0;
  }
  
  inline void observe(double time) {
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
  std::queue<std::pair<Arrival*, int>> queue;
  int queue_count;
  int queue_size;
  bool mon;
  ResStats* res_stats;
  
  inline bool room_in_server(int amount) { return server_count + amount < capacity; }
  inline bool room_in_queue(int amount) { return queue_count + amount < queue_size; }
};

#endif
