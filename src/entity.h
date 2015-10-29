#ifndef ENTITY_H
#define ENTITY_H

#include <Rcpp.h>
#include <queue>
#include <map>

// forward declaration
class Simulator;
class Resource;

/** 
 *  Base class. Every element in a simulation model is an entity.
 */
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

/** 
 * Class for holding processes.
 */
class Process: public Entity {
public:
  Process(Simulator* sim, Rcpp::Function func, bool mon): 
    Entity(sim, "dummy", mon), callback(func), resource(NULL), amount(0) {}
  virtual ~Process(){}
  Rcpp::Function callback;
  Resource* resource;
  int amount;
};

typedef std::queue<std::pair<Process*, int> > Queue;

/**
 * Resource statistics.
 */
class ResStats {
public:
  std::vector<double> time;
  std::vector<double> server;
  std::vector<double> queue;
};

/** 
 *  Generic resource, a passive entity that comprises server + FIFO queue.
 */
class Resource: public Entity {
public:
  
  /**
   * Constructor.
   * @param sim         a pointer to the simulator
   * @param name        the name
   * @param mon         bool that indicates whether this entity must be monitored
   * @param capacity    server capacity (-1 means infinity)
   * @param queue_size  room in the queue (-1 means infinity)
   */
  Resource(Simulator* sim, std::string name, bool mon, int capacity, int queue_size): 
    Entity(sim, name, mon), capacity(capacity), queue_size(queue_size), server_count(0), 
    queue_count(0) {
    res_stats = new ResStats();
  }
  
  ~Resource() {
    while (!queue.empty()) {
      delete queue.front().first;
      queue.pop();
    }
    delete res_stats;
  }
  
  /**
   * Reset the resource: server, queue and statistics.
   */
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
  
  /**
   * Request resources.
   * @param   amount  the amount of resources needed
   * 
   * @return  0 if success (serving now), failure otherwise (1: enqueued, -1: rejected)
   */
  int request(int amount);
  
  /**
   * Release resources.
   * 
   * @return  0 if success
   */
  int release();
  
  /**
   * Gather resource statistics.
   */
  void observe(double time) {
    res_stats->time.push_back(time);
    res_stats->server.push_back(server_count);
    res_stats->queue.push_back(queue_count);
  }
  
  ResStats* get_observations() { return res_stats; }
  int get_capacity() { return capacity; }
  int get_queue_size() { return queue_size; }
  
private:
  int capacity;
  int queue_size;
  int server_count;     /**< number of arrivals being served */
  int queue_count;      /**< number of arrivals waiting */
  Queue queue;          /**< queue container */
  ResStats* res_stats;  /**< resource statistics */
  
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
