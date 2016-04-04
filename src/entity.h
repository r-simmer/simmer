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
 * Abstract class for processes, active entities that need a method run().
 */
class Process: public Entity {
public:
  Process(Simulator* sim, std::string name, int mon, bool generator=false): 
    Entity(sim, name, mon), generator(generator), active(true) {}
  virtual ~Process(){}
  virtual void run() = 0;
  inline virtual void activate() { active = true; }
  inline virtual void deactivate() { active = false; }
  inline bool is_generator() { return generator; }
  inline bool is_active() { return active; }
private:
  bool generator;
  bool active;
};

typedef UMAP<std::string, double> Attr;

/** 
 *  Arrival process.
 */
class Arrival: public Process {
  struct ArrTime {
    double start;
    double activity;
    ArrTime(): start(-1), activity(0) {}
  };
  typedef UMAP<std::string, ArrTime> ResTime;
  
public:
  /**
   * Constructor.
   * @param sim             a pointer to the simulator
   * @param name            the name
   * @param mon             int that indicates whether this entity must be monitored
   * @param first_activity  the first activity of a user-defined R trajectory
   */
  Arrival(Simulator* sim, std::string name, int mon, Activity* first_activity, Generator* gen):
    Process(sim, name, mon), activity(first_activity), gen(gen), busy_until(-1), remaining(0) {}
  
  void run();
  void activate();
  void deactivate();
  
  int set_attribute(std::string key, double value);
  inline Attr* get_attributes() { return &attributes; }
  
  inline void set_start(std::string name, double start) { restime[name].start = start; }
  inline double get_start(std::string name) { return restime[name].start; }
  inline double get_start() { return lifetime.start; }
  
  inline void set_activity(std::string name, double act) { restime[name].activity = act; }
  inline double get_activity(std::string name) { return restime[name].activity; }
  inline double get_activity() { return lifetime.activity; }
  
  inline double get_remaining() { return remaining; }
  
  void leave(std::string name, double time);
  void reject(double time);

private:
  ArrTime lifetime;   /**< time spent in the whole trajectory */
  ResTime restime;    /**< time spent in resources */
  Activity* activity; /**< current activity from an R trajectory */
  Generator* gen;     /**< parent generator */
  Attr attributes;    /**< user-defined (key, value) pairs */
  double busy_until;  /**< next scheduled event time */
  double remaining;   /**< time remaining in a deactivated arrival */
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
  
  void run();
  
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

struct RSeize {
  double arrived_at;
  Arrival* arrival;
  int amount;
  int priority;
  int preemptible;
  bool restart;
  
  RSeize(double arrived_at, Arrival* arrival, int amount, int priority, 
         int preemptible, bool restart):
    arrived_at(arrived_at), arrival(arrival), amount(amount), priority(priority),
    preemptible(preemptible), restart(restart) {}
};

struct RQComp {
  bool operator()(const RSeize& lhs, const RSeize& rhs) const {
    if (lhs.priority == rhs.priority) {
      if (lhs.arrived_at == rhs.arrived_at)
        return lhs.arrival->get_remaining() > rhs.arrival->get_remaining();
      return lhs.arrived_at < rhs.arrived_at;
    }
    return lhs.priority > rhs.priority;
  }
};

struct RSCompFIFO {
  bool operator()(const RSeize& lhs, const RSeize& rhs) const {
    if (lhs.preemptible == rhs.preemptible)
      return lhs.arrived_at < rhs.arrived_at;
    return lhs.preemptible < rhs.preemptible;
  }
};

struct RSCompLIFO {
  bool operator()(const RSeize& lhs, const RSeize& rhs) const {
    if (lhs.preemptible == rhs.preemptible)
      return lhs.arrived_at > rhs.arrived_at;
    return lhs.preemptible < rhs.preemptible;
  }
};

typedef MSET<RSeize, RQComp> RPQueue;
typedef MSET<RSeize, RSCompFIFO> FIFO;
typedef MSET<RSeize, RSCompLIFO> LIFO;

/** 
*  Generic resource, a passive entity that comprises server + a priority queue.
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
    foreach_(RPQueue::value_type& itr, queue)
      delete itr.arrival;
    queue.clear();
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
  int seize(Arrival* arrival, int amount, int priority, int preemptible, bool restart);
  
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
  
protected:
  int capacity;
  int queue_size;
  int server_count;     /**< number of arrivals being served */
  int queue_count;      /**< number of arrivals waiting */
  RPQueue queue;        /**< queue container */
  StatsMap res_stats;   /**< resource statistics */
  
  virtual inline bool room_in_server(int amount, int priority) {
    if (capacity < 0) return true;
    return server_count + amount <= capacity;
  }
  
  virtual inline void insert_in_server(double time, Arrival* arrival, int amount, 
                                       int priority, int preemptible, bool restart) {
    server_count += amount;
  }
  
  virtual inline void remove_from_server(Arrival* arrival, int amount) {
    server_count -= amount;
  }
  
  virtual inline bool room_in_queue(int amount, int priority) {
    if (queue_size < 0) return true;
    if (queue_count + amount <= queue_size) return true;
    int count = 0;
    foreach_ (RPQueue::value_type& itr, queue) {
      if (priority > itr.priority)
        count += itr.amount;
      else break;
      if (count >= amount) return true;
    }
    return false;
  }
  
  virtual inline void insert_in_queue(double time, Arrival* arrival, int amount, 
                              int priority, int preemptible, bool restart) {
    if (queue_size > 0) while (queue_count + amount > queue_size) {
      RPQueue::iterator last = --queue.end();
      last->arrival->reject(time);
      queue_count -= last->amount;
      queue.erase(last);
    }
    queue_count += amount;
    queue.emplace(time, arrival, amount, priority, preemptible, restart);
  }
  
  virtual inline void serve_from_queue(double time) {
    RPQueue::iterator next = queue.begin();
    if (room_in_server(next->amount, next->priority)) {
      if (next->arrival->is_monitored()) {
        double last = next->arrival->get_activity(this->name);
        next->arrival->set_activity(this->name, time - last);
      }
      next->arrival->activate();
      insert_in_server(next->arrived_at, next->arrival, next->amount,
                       next->priority, next->preemptible, next->restart);
      queue_count -= next->amount;
      queue.erase(next);
    }
  }
};

/** 
*  Preemptive resource.
*/
template <typename T>
class PreemptiveResource: public Resource {
public:
  /**
  * Constructor.
  * @param preempt_order  "fifo" or "lifo"
  */
  PreemptiveResource(Simulator* sim, std::string name, int mon, int capacity, int queue_size):
    Resource(sim, name, mon, capacity, queue_size) {}
  
  ~PreemptiveResource() { reset(); }
  
  void reset() {
    Resource::reset();
    server.clear();
  }
  
protected:
  RPQueue preempted;    /**< preempted arrivals */
  T server;             /**< server container */
  //RPQueue queue;        // Why is this necessary???
  
  virtual inline bool room_in_server(int amount, int priority) {
    if (capacity < 0) return true;
    if (server_count + amount <= capacity) return true;
    int count = 0;
    foreach_ (typename T::value_type& itr, server) {
      if (priority > itr.preemptible)
        count += itr.amount;
      else break;
      if (count >= amount) return true;
    }
    return false;
  }
  
  virtual inline void insert_in_server(double time, Arrival* arrival, int amount, 
                               int priority, int preemptible, bool restart) {
    if (capacity > 0) while (server_count + amount > capacity) {
      typename T::iterator first = server.begin();
      first->arrival->deactivate();
      if (first->arrival->is_monitored()) {
        double last = first->arrival->get_activity(this->name);
        first->arrival->set_activity(this->name, time - last);
      }
      preempted.insert((*first));
      queue_count += first->amount;
      server_count -= first->amount;
      server.erase(first);
    }
    server_count += amount;
    server.emplace(time, arrival, amount, priority, preemptible, restart);
  }
  
  virtual inline void remove_from_server(Arrival* arrival, int amount) {
    typename T::iterator itr = server.begin();
    while (itr->arrival != arrival) ++itr;
    server.erase(itr);
    server_count -= amount;
  }
  
  virtual inline void serve_from_queue(double time) {
    RPQueue::iterator next;
    if (!preempted.empty()) next = preempted.begin();
    else next = queue.begin();
    if (room_in_server(next->amount, next->priority)) {
      if (next->arrival->is_monitored()) {
        double last = next->arrival->get_activity(this->name);
        next->arrival->set_activity(this->name, time - last);
      }
      next->arrival->activate();
      insert_in_server(next->arrived_at, next->arrival, next->amount,
                       next->priority, next->preemptible, next->restart);
      queue_count -= next->amount;
      if (!preempted.empty()) preempted.erase(next);
      else queue.erase(next);
    }
  }
};

#endif
