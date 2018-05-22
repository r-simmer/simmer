#ifndef simmer__process_arrival_impl_h
#define simmer__process_arrival_impl_h

#include <simmer/process/arrival.h>
#include <simmer/process/batched.h>
#include <simmer/resource.h>

inline void Arrival::terminate(bool finished) {
  foreach_ (ResMSet::value_type& itr, resources) {
    Rcpp::warning("'%s': leaving without releasing '%s'", name, itr->name);
    itr->erase(this, true);
  }
  unset_remaining();
  if (is_monitored() >= 1 && !dynamic_cast<Batched*>(this))
    sim->mon->record_end(name, lifetime.start, sim->now(), lifetime.activity, finished);
  delete this;
}

inline double Arrival::get_start(const std::string& name) {
  double start = restime[name].start;
  if (batch) {
    double up = batch->get_start(name);
    if (up >= 0 && (start < 0 || up < start))
      start = up;
  }
  return start;
}

inline void Arrival::register_entity(Resource* ptr) {
  if (is_monitored()) {
    restime[ptr->name].start = sim->now();
    restime[ptr->name].activity = 0;
  }
  resources.insert(ptr);
}

inline void Arrival::unregister_entity(Resource* ptr) {
  if (is_monitored())
    report(ptr->name);
  resources.erase(resources.find(ptr));
}

inline bool Arrival::leave_resources(bool flag) {
  if (status.busy_until > sim->now())
    unset_busy(sim->now());
  unset_remaining();
  while (resources.begin() != resources.end())
    flag |= (*resources.begin())->erase(this);
  return flag;
}

inline void Arrival::renege(Activity* next) {
  timer = NULL;
  cancel_renege();
  if (batch && !batch->erase(this))
    return;
  if (!leave_resources() && !batch)
    deactivate();
  batch = NULL;
  if (next) {
    activity = next;
    activate();
  } else terminate(false);
}

#endif
