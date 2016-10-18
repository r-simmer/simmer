#ifndef PROCESS_H
#define PROCESS_H

#include "entity.h"

// forward declarations
class Activity;
class Arrival;
class Batched;
class Resource;

/**
 * Abstract class for processes, active entities that need a method run().
 */
class Process : public Entity {
public:
  Process(Simulator* sim, std::string name, int mon, int priority = 0)
    : Entity(sim, name, mon), active(true), priority(priority) {}
  virtual void run() = 0;
  virtual bool activate(double delay = 0);
  virtual bool deactivate();

protected:
  bool active;
  int priority;
};

class Manager : public Process {
  typedef boost::function<void (int)> Setter;

public:
  Manager(Simulator* sim, std::string name, std::string param,
          VEC<double> duration, VEC<int> value, int period, Setter set)
    : Process(sim, name, false, PRIORITY_MANAGER), param(param),
      duration(duration), value(value), period(period), set(set), index(0) {}

  void reset() { index = 0; }
  void run();
  bool activate(double delay = 0) { return Process::activate(duration[index]); }

private:
  std::string param;
  VEC<double> duration;
  VEC<int> value;
  int period;
  Setter set;
  unsigned int index;
};

class Task : public Process {
  typedef boost::function<void ()> Bind;

public:
  Task(Simulator* sim, std::string name, Bind task, int priority = 0)
    : Process(sim, name, false, priority), task(task) {}
  ~Task() { reset(); }

  void reset() {}
  void run();

private:
  Bind task;
};

typedef UMAP<std::string, double> Attr;

struct Order {
public:
  Order(int priority=0, int preemptible=0, bool restart=false)
    : preemptible(preemptible)
  {
    set_priority(priority);
    set_preemptible(preemptible);
    set_restart(restart);
  }

  void set_priority(int value) {
    priority = value;
    if (preemptible < priority)
      preemptible = priority;
  }
  int get_priority() { return priority; }
  void set_preemptible(int value) {
    if (value < priority) {
      Rcpp::warning("`preemptible` level cannot be < `priority`, `preemptible` set to %d", priority);
      value = priority;
    }
    preemptible = value;
  }
  int get_preemptible() { return preemptible; }
  void set_restart(bool value) { restart = value; }
  bool get_restart() { return restart; }

private:
  int priority;       /**< arrival priority */
  int preemptible;    /**< maximum priority that cannot cause preemption (>=priority) */
  bool restart;       /**< whether activity must be restarted after preemption */
};

/**
 * Generation process.
 */
class Generator : public Process {
public:
  /**
   * Constructor.
   * @param sim             a pointer to the simulator
   * @param name            the name
   * @param mon             int that indicates whether this entity must be monitored
   * @param trj             a user-defined R trajectory
   * @param dist            a user-defined R function that provides random numbers
   * @param order           priority, preemptible, restart
   */
  Generator(Simulator* sim, std::string name_prefix, int mon,
            Rcpp::Environment trj, Rcpp::Function dist, Order order)
    : Process(sim, name_prefix, mon, PRIORITY_GENERATOR), count(0), trj(trj),
      dist(dist), order(order), first_activity(NULL) { set_first_activity(); }

  /**
   * Reset the generator: counter, trajectory
   */
  void reset() {
    count = 0;
    Rcpp::Environment dist_env(dist.environment());
    Rcpp::Environment reset_env(dist_env[".reset"]);
    Rcpp::Function reset_fun(reset_env["reset"]);
    reset_fun();
  }

  void run();

  int get_n_generated() { return count; }
  void set_trajectory(Rcpp::Environment new_trj) {
    trj = new_trj;
    set_first_activity();
  }
  void set_distribution(Rcpp::Function new_dist) { dist = new_dist; }

private:
  int count;                /**< number of arrivals generated */
  Rcpp::Environment trj;
  Rcpp::Function dist;
  Order order;
  Activity* first_activity;

  void set_first_activity();
};

/**
 *  Arrival process.
 */
class Arrival : public Process {
public:
  struct ArrTime {
    double start;
    double activity;
    double busy_until;
    double remaining;
    ArrTime() : start(-1), activity(0), busy_until(-1), remaining(0) {}
  };
  typedef UMAP<std::string, ArrTime> ResTime;
  typedef UMAP<int, Resource*> SelMap;
  typedef MSET<Resource*> ResMSet;

  CLONEABLE_COUNT(Arrival)

  Order order;        /**< priority, preemptible, restart */

  /**
  * Constructor.
  * @param sim             a pointer to the simulator
  * @param name            the name
  * @param mon             int that indicates whether this entity must be monitored
  * @param order           priority, preemptible, restart
  * @param first_activity  the first activity of a user-defined R trajectory
  */
  Arrival(Simulator* sim, std::string name, int mon, Order order,
          Activity* first_activity, int priority = 0)
    : Process(sim, name, mon, priority), clones(new int(1)), order(order),
      activity(first_activity), timer(NULL), batch(NULL) {}

  Arrival(const Arrival& o)
    : Process(o), clones(o.clones), order(o.order), activity(NULL), timer(NULL), batch(NULL) {}

  ~Arrival() { reset(); }

  void reset();
  void run();
  void restart();
  void interrupt();
  virtual void leave(std::string resource);
  virtual void leave(std::string resource, double start, double activity);
  virtual void terminate(bool finished);
  void renege(Activity* next);
  virtual int set_attribute(std::string key, double value);
  double get_start(std::string name);

  Attr* get_attributes() { return &attributes; }
  double get_remaining() { return lifetime.remaining; }
  void set_start(std::string name, double value) { restime[name].start = value; }
  void set_activity(Activity* ptr) { activity = ptr; }
  void set_activity(double value) { lifetime.activity = value; }
  void set_activity(std::string name, double value) { restime[name].activity = value; }
  double get_start() { return lifetime.start; }
  Activity* get_current() { return activity; }
  double get_activity() { return lifetime.activity - lifetime.remaining; }
  double get_activity(std::string name) { return restime[name].activity; }

  void set_selected(int id, Resource* res) { selected[id] = res; }
  Resource* get_selected(int id) { return selected[id]; }
  void register_entity(Resource* ptr) { resources.insert(ptr); }
  void register_entity(Batched* ptr) { batch = ptr; }
  void unregister_entity(Resource* ptr) { resources.erase(resources.find(ptr)); }
  void unregister_entity(Batched* ptr) { batch = NULL; }

  void set_timeout(double timeout, Activity* next) {
    cancel_timeout();
    timer = new Task(sim, "Renege-Timer",
                     boost::bind(&Arrival::renege, this, next),
                     PRIORITY_MIN);
    timer->activate(timeout);
  }

  void cancel_timeout() {
    if (!timer)
      return;
    timer->deactivate();
    delete timer;
    timer = NULL;
  }

protected:
  ArrTime lifetime;     /**< time spent in the whole trajectory */
  ResTime restime;      /**< time spent in resources */
  Activity* activity;   /**< current activity from an R trajectory */
  Attr attributes;      /**< user-defined (key, value) pairs */
  SelMap selected;      /**< selected resource */
  Task* timer;          /**< timer that triggers reneging */
  Batched* batch;       /**< batch that contains this arrival */
  ResMSet resources;    /**< resources that contain this arrival */
};

/**
 *  Batch of arrivals.
 */
class Batched : public Arrival {
public:
  CLONEABLE_COUNT_DERIVED(Batched)

  Batched(Simulator* sim, std::string name, bool permanent, int priority = 0)
    : Arrival(sim, name, true, Order(), NULL, priority), permanent(permanent) {}

  Batched(const Batched& o) : Arrival(o), arrivals(o.arrivals), permanent(o.permanent) {
    for (unsigned int i=0; i<arrivals.size(); i++) {
      arrivals[i] = arrivals[i]->clone();
      arrivals[i]->register_entity(this);
    }
  }

  ~Batched() { reset(); }

  void reset() {
    foreach_ (VEC<Arrival*>::value_type& itr, arrivals)
      delete itr;
    arrivals.clear();
  }

  void leave(std::string resource) {
    foreach_ (VEC<Arrival*>::value_type& itr, arrivals) {
      if (itr->is_monitored())
        itr->leave(resource, restime[resource].start, restime[resource].activity);
    }
  }

  void leave(std::string resource, double start, double activity) {
    foreach_ (VEC<Arrival*>::value_type& itr, arrivals) {
      if (itr->is_monitored())
        itr->leave(resource, start, activity);
    }
  }

  void terminate(bool finished);

  void pop_all(Activity* next) {
    foreach_ (Arrival* arrival, arrivals) {
      arrival->set_activity(arrival->get_activity() + lifetime.activity);
      arrival->set_activity(next);
      arrival->unregister_entity(this);
      arrival->activate();
    }
    arrivals.clear();
  }

  int set_attribute(std::string key, double value);

  bool is_permanent() { return permanent; }
  size_t size() { return arrivals.size(); }

  void insert(Arrival* arrival) {
    arrivals.push_back(arrival);
    arrival->register_entity(this);
  }
  void erase(Arrival* arrival);
  void report(Arrival* arrival);

protected:
  VEC<Arrival*> arrivals;
  bool permanent;
};

#endif
