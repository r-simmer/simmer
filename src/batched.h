#ifndef BATCHED_H
#define BATCHED_H

#include "arrival.h"

/**
 *  Batch of arrivals.
 */
class Batched : public Arrival {
public:
  CLONEABLE(Batched)

  Batched(Simulator* sim, const std::string& name, bool permanent, int priority = 0)
    : Arrival(sim, name, true, Order(), NULL, priority), permanent(permanent) {}

  Batched(const Batched& o) : Arrival(o), arrivals(o.arrivals), permanent(o.permanent) {
    for (size_t i=0; i<arrivals.size(); i++) {
      arrivals[i] = arrivals[i]->clone();
      arrivals[i]->register_entity(this);
    }
  }

  ~Batched() { reset(); }

  void terminate(bool finished);

  void pop_all(Activity* next) {
    foreach_ (Arrival* arrival, arrivals) {
      arrival->set_activity(next);
      arrival->unregister_entity(this);
      arrival->activate();
    }
    arrivals.clear();
  }

  void set_attribute(const std::string& key, double value, bool global=false);

  bool is_permanent() const { return permanent; }
  size_t size() const { return arrivals.size(); }

  void insert(Arrival* arrival) {
    arrival->set_activity(NULL);
    arrivals.push_back(arrival);
    arrival->register_entity(this);
  }
  void erase(Arrival* arrival);

private:
  VEC<Arrival*> arrivals;
  bool permanent;

  void reset() {
    foreach_ (Arrival* arrival, arrivals)
      delete arrival;
    arrivals.clear();
  }

  void report(const std::string& resource) const {
    foreach_ (const Arrival* arrival, arrivals) {
      if (arrival->is_monitored()) {
        ArrTime time = restime.find(resource)->second;
        arrival->report(resource, time.start, time.activity);
      }
    }
  }

  void report(const std::string& resource, double start, double activity) const {
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
