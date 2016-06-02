#ifndef RESOURCE_H
#define RESOURCE_H

#include "process.h"

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
  
  /**
  * Reset the resource: server, queue and statistics.
  */
  virtual void reset() {
    server_count = 0;
    queue_count = 0;
    foreach_ (RPQueue::value_type& itr, queue)
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
  void observe(double time) {
    res_stats.insert("time",        time);
    res_stats.insert("server",      server_count);
    res_stats.insert("queue",       queue_count);
    res_stats.insert("capacity",    capacity);
    res_stats.insert("queue_size",  queue_size);
  }
  
  StatsMap* get_observations() { return &res_stats; }
  void set_capacity(int value);
  int get_capacity() { return capacity; }
  void set_queue_size(int value);
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
  
  void verbose_print(double time, std::string arrival, std::string status) {
    Rcpp::Rcout << 
      FMT_0 << time << " |" << FMT_11 << "resource: " << FMT_12 << name << "|" << 
      FMT_21 << "arrival: " << FMT_22 << arrival << "| " << status << std::endl;
  }
  
  virtual bool room_in_server(int amount, int priority) {
    if (capacity < 0) return true;
    return server_count + amount <= capacity;
  }
  
  virtual bool try_free_server(bool verbose, double time) { return false; }
  
  virtual void insert_in_server(bool verbose, double time, Arrival* arrival, int amount, 
                                int priority, int preemptible, bool restart) {
    if (verbose) verbose_print(time, arrival->name, "SERVE");
    server_count += amount;
  }
  
  virtual void remove_from_server(bool verbose, double time, Arrival* arrival, int amount) {
    if (verbose) verbose_print(time, arrival->name, "DEPART");
    server_count -= amount;
  }
  
  virtual bool room_in_queue(int amount, int priority) {
    if (queue_size < 0) return true;
    if (queue_count + amount <= queue_size) return true;
    int count = 0;
    foreach_r_ (RPQueue::value_type& itr, queue) {
      if (priority > itr.priority)
        count += itr.amount;
      else break;
      if (count >= amount) return true;
    }
    return false;
  }
  
  virtual void insert_in_queue(bool verbose, double time, Arrival* arrival, int amount, 
                               int priority, int preemptible, bool restart) {
    int count = 0;
    if (queue_size > 0) while (queue_count + amount > queue_size && amount > count) {
      RPQueue::iterator last = --queue.end();
      if (verbose) verbose_print(time, last->arrival->name, "REJECT");
      last->arrival->terminate(time, false);
      queue_count -= last->amount;
      count += last->amount;
      queue.erase(last);
    }
    if (verbose) verbose_print(time, arrival->name, "ENQUEUE");
    queue_count += amount;
    queue.emplace(time, arrival, amount, priority, preemptible, restart);
  }
  
  virtual bool try_serve_from_queue(bool verbose, double time) {
    RPQueue::iterator next = queue.begin();
    if (room_in_server(next->amount, next->priority)) {
      if (next->arrival->is_monitored()) {
        double last = next->arrival->get_activity(this->name);
        next->arrival->set_activity(this->name, time - last);
      }
      next->arrival->activate();
      insert_in_server(verbose, time, next->arrival, next->amount,
                       next->priority, next->preemptible, next->restart);
      queue_count -= next->amount;
      queue.erase(next);
      return true;
    }
    return false;
  }
};

/** 
*  Preemptive resource.
*/
template <typename T>
class PreemptiveResource: public Resource {
public:
  PreemptiveResource(Simulator* sim, std::string name, int mon, int capacity, int queue_size):
    Resource(sim, name, mon, capacity, queue_size) {}
  
  virtual void reset() {
    Resource::reset();
    foreach_ (RPQueue::value_type& itr, preempted)
      delete itr.arrival;
    preempted.clear();
    server.clear();
  }
  
protected:
  RPQueue preempted;    /**< preempted arrivals */
  T server;             /**< server container */
  
  virtual bool room_in_server(int amount, int priority) {
    if (capacity < 0) return true;
    if (server_count + amount <= capacity) return true;
    int count = 0;
    foreach_ (typename T::value_type& itr, server) {
      if (priority >= itr.preemptible)
        count += itr.amount;
      else break;
      if (count >= amount) return true;
    }
    return false;
  }
  
  virtual bool try_free_server(bool verbose, double time) {
    typename T::iterator first = server.begin();
    first->arrival->deactivate(first->restart);
    if (verbose) verbose_print(time, first->arrival->name, "PREEMPT");
    if (first->arrival->is_monitored()) {
      double last = first->arrival->get_activity(this->name);
      first->arrival->set_activity(this->name, time - last);
    }
    preempted.insert((*first));
    queue_count += first->amount;
    server_count -= first->amount;
    server.erase(first);
    return true;
  }
  
  virtual void insert_in_server(bool verbose, double time, Arrival* arrival, int amount, 
                                int priority, int preemptible, bool restart) {
    if (capacity > 0) while (server_count + amount > capacity)
      try_free_server(verbose, time);
    if (verbose) verbose_print(time, arrival->name, "SERVE");
    server_count += amount;
    server.emplace(time, arrival, amount, priority, preemptible, restart);
  }
  
  virtual void remove_from_server(bool verbose, double time, Arrival* arrival, int amount) {
    if (verbose) verbose_print(time, arrival->name, "DEPART");
    typename T::iterator itr = server.begin();
    while (itr->arrival != arrival) ++itr;
    server.erase(itr);
    server_count -= amount;
  }
  
  virtual bool try_serve_from_queue(bool verbose, double time) {
    RPQueue::iterator next;
    bool flag = false;
    if (!preempted.empty()) {
      next = preempted.begin();
      flag = true;
    }
    else next = queue.begin();
    if (room_in_server(next->amount, next->priority)) {
      if (next->arrival->is_monitored()) {
        double last = next->arrival->get_activity(this->name);
        next->arrival->set_activity(this->name, time - last);
      }
      next->arrival->activate();
      insert_in_server(verbose, time, next->arrival, next->amount,
                       next->priority, next->preemptible, next->restart);
      queue_count -= next->amount;
      if (flag) preempted.erase(next);
      else queue.erase(next);
      return true;
    }
    return false;
  }
};

#endif
