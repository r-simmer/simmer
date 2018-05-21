#include <simmer/process/arrival.h>
#include <simmer/process/batched.h>
#include <simmer/simulator.h>
#include <simmer/activity.h>
#include <simmer/resource.h>

void Arrival::init() {
  (*clones)++;
  sim->register_arrival(this);
}

void Arrival::reset() {
  cancel_renege();
  if (!--(*clones))
    delete clones;
  sim->unregister_arrival(this);
}

void Arrival::run() {
  double delay;

  if (!activity)
    goto finish;
  if (lifetime.start < 0)
    lifetime.start = sim->now();

  if (sim->verbose) {
    Rcpp::Rcout <<
      FMT(10, right) << sim->now() << " |" <<
      FMT(12, right) << "arrival: " << FMT(15, left) << name << "|" <<
      FMT(12, right) << "activity: " << FMT(15, left) << activity->name << "| ";
    activity->print(0, false, true);
  }

  delay = activity->run(this);
  if (delay == REJECT)
    goto end;
  activity = activity->get_next();
  if (delay == ENQUEUE)
    goto end;

  if (delay != BLOCK) {
    set_busy(sim->now() + delay);
    update_activity(delay);
  }
  sim->schedule(delay, this, activity ? activity->priority : PRIORITY_MAX);

end:
  return;
finish:
  terminate(true);
}

void Arrival::restart() {
  set_busy(sim->now() + status.remaining);
  activate(status.remaining);
  set_remaining(0);
  paused = false;
}

void Arrival::pause() {
  deactivate();
  unset_busy(sim->now());
  if (status.remaining && order.get_restart()) {
    unset_remaining();
    activity = activity->get_prev();
  }
  paused = true;
}

void Arrival::stop() {
  deactivate();
  if (status.busy_until < sim->now())
    return;
  unset_busy(sim->now());
  unset_remaining();
}

void Arrival::terminate(bool finished) {
  foreach_ (ResMSet::value_type& itr, resources) {
    Rcpp::warning("'%s': leaving without releasing '%s'", name, itr->name);
    itr->erase(this, true);
  }
  unset_remaining();
  if (is_monitored() >= 1 && !dynamic_cast<Batched*>(this))
    sim->mon->record_end(name, lifetime.start, sim->now(), lifetime.activity, finished);
  delete this;
}

void Arrival::set_attribute(const std::string& key, double value, bool global) {
  if (global) return sim->set_attribute(key, value);
  attributes[key] = value;
  if (is_monitored() >= 2)
    sim->mon->record_attribute(sim->now(), name, key, value);
}

double Arrival::get_attribute(const std::string& key, bool global) const {
  if (global) return sim->get_attribute(key);
  Attr::const_iterator search = attributes.find(key);
  if (search == attributes.end())
    return NA_REAL;
  return search->second;
}

double Arrival::get_start(const std::string& name) {
  double start = restime[name].start;
  if (batch) {
    double up = batch->get_start(name);
    if (up >= 0 && (start < 0 || up < start))
      start = up;
  }
  return start;
}

void Arrival::register_entity(Resource* ptr) {
  if (is_monitored()) {
    restime[ptr->name].start = sim->now();
    restime[ptr->name].activity = 0;
  }
  resources.insert(ptr);
}

void Arrival::unregister_entity(Resource* ptr) {
  if (is_monitored())
    report(ptr->name);
  resources.erase(resources.find(ptr));
}

void Arrival::set_renege(const std::string& sig, Activity* next) {
  cancel_renege();
  signal = sig;
  sim->subscribe(signal, this, BIND(&Arrival::renege, this, next));
}

void Arrival::cancel_renege() {
  if (timer) {
    timer->deactivate();
    delete timer;
    timer = NULL;
  } else if (!signal.empty()) {
    sim->unsubscribe(signal, this);
    signal.clear();
  }
}

void Arrival::renege(Activity* next) {
  timer = NULL;
  cancel_renege();
  if (batch) {
    if (batch->is_permanent())
      return;
    batch->erase(this);
  }
  if (!leave_resources() && !batch)
    deactivate();
  if (next) {
    activity = next;
    activate();
  } else terminate(false);
}

void Arrival::report(const std::string& resource) const {
  ArrTime time = restime.find(resource)->second;
  sim->mon->record_release(name, time.start, sim->now(), time.activity, resource);
}

void Arrival::report(const std::string& resource, double start, double activity) const {
  sim->mon->record_release(name, start, sim->now(), activity, resource);
}

bool Arrival::leave_resources(bool flag) {
  if (status.busy_until > sim->now())
    unset_busy(sim->now());
  unset_remaining();
  while (resources.begin() != resources.end())
    flag |= (*resources.begin())->erase(this);
  return flag;
}
