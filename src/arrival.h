#ifndef ARRIVAL_H
#define ARRIVAL_H

#include "process.h"
#include "task.h"
#include "order.h"

// forward declarations
class Activity;
class Batched;
class Resource;

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

  CLONEABLE(Arrival)

  Order order;        /**< priority, preemptible, restart */

  /**
  * Constructor.
  * @param sim             a pointer to the simulator
  * @param name            the name
  * @param mon             int that indicates whether this entity must be monitored
  * @param order           priority, preemptible, restart
  * @param first_activity  the first activity of a user-defined R trajectory
  */
  Arrival(Simulator* sim, const std::string& name, int mon, Order order,
          Activity* first_activity, int priority = 0)
    : Process(sim, name, mon, priority), order(order), paused(false),
      clones(new int(0)), activity(first_activity), timer(NULL), batch(NULL)
      { init(); }

  Arrival(const Arrival& o)
    : Process(o), order(o.order), paused(o.paused), clones(o.clones),
      activity(NULL), attributes(o.attributes), timer(NULL), batch(NULL)
      { init(); }

  ~Arrival() { reset(); }

  void run();
  void restart();
  void pause();
  bool is_paused() const { return paused; }
  void stop();
  virtual void terminate(bool finished);

  int get_clones() const { return *clones; }

  virtual void set_attribute(const std::string& key, double value, bool global=false);
  double get_attribute(const std::string& key, bool global=false) const;

  double get_start(const std::string& name);
  double get_start() const { return lifetime.start; }
  double get_remaining() const { return status.remaining; }

  void set_activity(Activity* ptr) { activity = ptr; }
  Activity* get_activity() const { return activity; }

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
                     BIND(&Arrival::renege, this, next),
                     PRIORITY_MIN);
    timer->activate(timeout);
  }
  void set_renege(const std::string& sig, Activity* next);
  void cancel_renege();

private:
  bool paused;
  int* clones;          /**< number of active clones */
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

  void init();
  void reset();
  void renege(Activity* next);
  virtual void report(const std::string& resource) const;
  virtual void report(const std::string& resource, double start, double activity) const;
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

#endif
