#include "process.h"
#include "simulator.h"
#include "activity.h"

bool Process::activate(double delay) {
  sim->schedule(delay, this, priority);
  return true;
}

bool Process::deactivate() {
  if (!sim->is_scheduled(this))
    return false;
  sim->unschedule(this);
  return true;
}

Activity* trj_head(const REnv& trj) { return trj_get(trj, "head"); }

Arrival* Source::new_arrival(double delay) {
  // format the name and create the next arrival
  std::string arr_name = name + boost::lexical_cast<std::string>(count++);
  Arrival* arrival = new Arrival(sim, arr_name, is_monitored(),
                                 order, first_activity, count);

  if (sim->verbose) Rcpp::Rcout <<
    FMT(10, right) << sim->now() << " |" <<
    FMT(12, right) << "source: " << FMT(15, left) << name << "|" <<
    FMT(12, right) << "new: " << FMT(15, left) << arr_name << "| " <<
    (sim->now() + delay) << std::endl;

  return arrival;
}

void Generator::run() {
  // get the delay for the next (n) arrival(s)
  RNum delays = source();
  size_t n = delays.size();
  double delay = 0;

  for (size_t i = 0; i < n; ++i) {
    if (delays[i] < 0)
      return;
    delay += delays[i];

    // schedule the arrival
    sim->schedule(delay, new_arrival(delay),
                  first_activity->priority ? first_activity->priority : count);
  }
  // schedule the generator
  sim->schedule(delay, this, priority);
}

void DataSrc::run() {
  double delay = 0;
  RNum col, time = source[col_time];
  int i = 0;

  while (i++ != batch) {
    if (time.size() <= count)
      return;
    delay += time[count];

    Arrival* arrival = new_arrival(delay);

    for (size_t j = 0; j < col_attrs.size(); ++j) {
      col = source[col_attrs[j]];
      arrival->set_attribute(col_attrs[j], col[count-1]);
    }

    if (col_priority) {
      col = source[*col_priority];
      arrival->order.set_priority(col[count-1]);
    }
    if (col_preemptible) {
      col = source[*col_preemptible];
      arrival->order.set_preemptible(col[count-1]);
    }
    if (col_restart) {
      col = source[*col_restart];
      arrival->order.set_restart(col[count-1]);
    }

    // schedule the arrival
    sim->schedule(delay, arrival,
                  first_activity->priority ? first_activity->priority : count);
  }
  // schedule the generator
  sim->schedule(delay, this, priority);
}

void Manager::run() {
  if (sim->verbose) Rcpp::Rcout <<
    FMT(10, right) << sim->now() << " |" <<
    FMT(12, right) << "manager: " << FMT(15, left) << name << "|" <<
    FMT(12, right) << "parameter: " << FMT(15, left) << param << "| " <<
    value[index] << std::endl;

  set(value[index]);
  index++;
  if (index == duration.size()) {
    if (period < 0)
      goto end;
    index = 1;
  }

  sim->schedule(duration[index], this, priority);
end:
  return;
}

void Task::run() {
  if (sim->verbose) Rcpp::Rcout <<
    FMT(10, right) << sim->now() << " |" <<
    FMT(12, right) << "task: " << FMT(15, left) << name << "|" <<
    FMT(12+16, right) << "|" << std::endl;

  task();
  delete this;
}

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
    sim->record_end(name, lifetime.start, lifetime.activity, finished);
  delete this;
}

void Arrival::set_attribute(const std::string& key, double value, bool global) {
  if (global) return sim->set_attribute(key, value);
  attributes[key] = value;
  if (is_monitored() >= 2)
    sim->record_attribute(name, key, value);
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
  sim->record_release(name, time.start, time.activity, resource);
}

void Arrival::report(const std::string& resource, double start, double activity) const {
  sim->record_release(name, start, activity, resource);
}

bool Arrival::leave_resources(bool flag) {
  if (status.busy_until > sim->now())
    unset_busy(sim->now());
  unset_remaining();
  while (resources.begin() != resources.end())
    flag |= (*resources.begin())->erase(this);
  return flag;
}

void Batched::terminate(bool finished) {
  foreach_ (Arrival* arrival, arrivals)
    arrival->terminate(finished);
  arrivals.clear();
  Arrival::terminate(finished);
}

void Batched::set_attribute(const std::string& key, double value, bool global) {
  if (global) return sim->set_attribute(key, value);
  attributes[key] = value;
  foreach_ (Arrival* arrival, arrivals)
    arrival->set_attribute(key, value);
}

void Batched::erase(Arrival* arrival) {
  bool del = activity;
  if (arrivals.size() > 1 || (batch && batch->is_permanent())) {
    del = false;
    if (arrival->is_monitored()) {
      Batched* up = this;
      while (up) {
        up->report(arrival);
        up = up->batch;
      }
    }
  } else if (arrivals.size() == 1 && !batch) {
    if (!leave_resources(!activity))
      deactivate();
  } else {
    del = true;
    batch->erase(this);
    leave_resources();
  }
  arrivals.erase(std::remove(arrivals.begin(), arrivals.end(), arrival), arrivals.end());
  arrival->unregister_entity(this);
  if (del) delete this;
}

void Batched::report(Arrival* arrival) const {
  foreach_ (const ResTime::value_type& itr, restime)
    arrival->report(itr.first, itr.second.start,
                    itr.second.activity - status.busy_until + sim->now());
}
