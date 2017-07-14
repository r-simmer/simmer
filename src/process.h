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
  bool is_active() const { return active; }

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
  size_t index;
};

class Task : public Process {
public:
  Task(Simulator* sim, std::string name, BIND(void) task, int priority = 0)
    : Process(sim, name, false, priority), task(task) {}
  ~Task() { reset(); }

  void reset() {}
  void run();

private:
  BIND(void) task;
};

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
  int get_priority() const { return priority; }
  void set_preemptible(int value) {
    if (value < priority) {
      Rcpp::warning("`preemptible` level cannot be < `priority`, `preemptible` set to %d", priority);
      value = priority;
    }
    preemptible = value;
  }
  int get_preemptible() const { return preemptible; }
  void set_restart(bool value) { restart = value; }
  bool get_restart() const { return restart; }

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

  int get_n_generated() const { return count; }
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
  friend class Batched;

public:
  struct ArrTime {
    double start;
    double activity;
    ArrTime() : start(-1), activity(0) {}
  };
  struct ArrStatus {
    double busy_until;
    double remaining;
    ArrStatus() : busy_until(-1), remaining(0) {}
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
  void pause();
  void stop();
  virtual void terminate(bool finished);
  virtual void set_attribute(std::string key, double value);
  double get_start(std::string name);

  double get_remaining() const { return status.remaining; }
  void set_activity(Activity* ptr) { activity = ptr; }
  double get_start() const { return lifetime.start; }
  Activity* get_activity() const { return activity; }
  Attr* get_attributes() { return &attributes; }
  double get_attribute(std::string key) const {
    Attr::const_iterator search = attributes.find(key);
    if (search == attributes.end())
      return NA_REAL;
    return (*search).second;
  }

  void set_resource_selected(int id, Resource* res) { selected[id] = res; }
  Resource* get_resource_selected(int id) const {
    SelMap::const_iterator search = selected.find(id);
    if (search != selected.end())
      return search->second;
    return NULL;
  }
  void register_entity(Resource* ptr);
  void register_entity(Batched* ptr) { batch = ptr; }
  void unregister_entity(Resource* ptr);
  void unregister_entity(Batched* ptr) { batch = NULL; }

  void set_renege(double timeout, Activity* next) {
    cancel_renege();
    timer = new Task(sim, "Renege-Timer",
                     boost::bind(&Arrival::renege, this, next),
                     PRIORITY_MIN);
    timer->activate(timeout);
  }
  void set_renege(std::string sig, Activity* next);
  void cancel_renege();

protected:
  ArrStatus status;     /**< arrival timing status */
  ArrTime lifetime;     /**< time spent in the whole trajectory */
  ResTime restime;      /**< time spent in resources */
  Activity* activity;   /**< current activity from an R trajectory */
  Attr attributes;      /**< user-defined (key, value) pairs */
  SelMap selected;      /**< selected resource */
  Task* timer;          /**< timer that triggers reneging */
  std::string signal;   /**< signal that triggers reneging */
  Batched* batch;       /**< batch that contains this arrival */
  ResMSet resources;    /**< resources that contain this arrival */

  void renege(Activity* next);
  virtual void report(std::string resource) const;
  virtual void report(std::string resource, double start, double activity) const;
  bool leave_resources(bool flag = false);

  virtual void update_activity(double value) {
    lifetime.activity += value;
    if (is_monitored()) {
      foreach_ (ResTime::value_type& itr, restime)
      itr.second.activity += value;
    }
  }

  virtual void set_remaining(double value) {
    status.remaining = value;
  }

  virtual void set_busy(double value) {
    status.busy_until = value;
  }

  void unset_remaining() {
    update_activity(-status.remaining);
    set_remaining(0);
  }

  void unset_busy(double now) {
    set_remaining(status.busy_until - now);
    set_busy(0);
  }
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
    for (size_t i=0; i<arrivals.size(); i++) {
      arrivals[i] = arrivals[i]->clone();
      arrivals[i]->register_entity(this);
    }
  }

  ~Batched() { reset(); }

  void reset() {
    foreach_ (Arrival* arrival, arrivals)
      delete arrival;
    arrivals.clear();
  }

  void terminate(bool finished);

  void pop_all(Activity* next) {
    foreach_ (Arrival* arrival, arrivals) {
      arrival->set_activity(next);
      arrival->unregister_entity(this);
      arrival->activate();
    }
    arrivals.clear();
  }

  void set_attribute(std::string key, double value);

  bool is_permanent() const { return permanent; }
  size_t size() const { return arrivals.size(); }

  void insert(Arrival* arrival) {
    arrival->set_activity(NULL);
    arrivals.push_back(arrival);
    arrival->register_entity(this);
  }
  void erase(Arrival* arrival);

protected:
  VEC<Arrival*> arrivals;
  bool permanent;

  void report(std::string resource) const {
    foreach_ (const Arrival* arrival, arrivals) {
      if (arrival->is_monitored()) {
        ArrTime time = restime.find(resource)->second;
        arrival->report(resource, time.start, time.activity);
      }
    }
  }

  void report(std::string resource, double start, double activity) const {
    foreach_ (const Arrival* arrival, arrivals) {
      if (arrival->is_monitored())
        arrival->report(resource, start, activity);
    }
  }

  void report(Arrival* arrival) const;

  void update_activity(double value) {
    Arrival::update_activity(value);
    foreach_ (Arrival* arrival, arrivals)
      arrival->update_activity(value);
  }

  void set_remaining(double value) {
    Arrival::set_remaining(value);
    foreach_ (Arrival* arrival, arrivals)
      arrival->set_remaining(value);
  }

  void set_busy(double value) {
    Arrival::set_busy(value);
    foreach_ (Arrival* arrival, arrivals)
      arrival->set_busy(value);
  }
};

#endif
