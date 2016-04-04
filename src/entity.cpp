#include "entity.h"
#include "simulator.h"
#include "activity.h"

inline void Arrival::run() {
  double delay;
  
  if (!is_active()) goto end;
  if (!activity) goto finish;
  if (lifetime.start < 0) lifetime.start = sim->now();
  if (sim->verbose) Rcpp::Rcout <<
    "sim: " << sim->name << " | " << "time: " << sim->now() << " | " <<
    "arrival: " << name << " | " << "activity: " << 
    activity->name << "(" << activity->resource << ")" << std::endl;
  
  delay = activity->run(this);
  if (delay == REJECTED) goto end;
  activity = activity->get_next();
  if (delay == ENQUEUED) goto end;
  
  busy_until = sim->now() + delay;
  lifetime.activity += delay;
  sim->schedule(delay, this, activity ? activity->priority : 0);
  goto end;
  
finish:
  gen->notify_end(sim->now(), this, true);
end:
  return;
}

inline void Arrival::activate() {
  Process::activate();
  busy_until = sim->now() + remaining;
  sim->schedule(remaining, this, activity ? activity->priority : 0);
  remaining = 0;
}

inline void Arrival::deactivate() {
  Process::deactivate();
  remaining = busy_until - sim->now();
}

inline void Arrival::leave(std::string name, double time) {
  gen->notify_release(time, this, name);
}

void Arrival::reject(double time) {
  gen->notify_end(time, this, false);
}

void Generator::run() {
  if (!is_active()) return;
  
  // get the delay for the next arrival
  double delay = Rcpp::as<double>(dist());
  if (delay < 0) return;
  
  // format the name and create the next arrival
  char numstr[21];
  sprintf(numstr, "%d", count);
  Arrival* arrival = new Arrival(sim, name + numstr, is_monitored(), first_activity, this);
  
  // schedule the arrival and the generator itself
  sim->schedule(delay, arrival);
  sim->schedule(delay, this);
  
  count++;
}

int Arrival::set_attribute(std::string key, double value) {
  attributes[key] = value;
  
  if (is_monitored() >= 2) gen->observe(sim->now(), this, key);
  return 0;
}

int Resource::seize(Arrival* arrival, int amount, int priority, int preemptible, bool restart) {
  int status;
  // serve now
  if (room_in_server(amount, priority)) {
    if (arrival->is_monitored()) {
      arrival->set_start(this->name, sim->now());
      arrival->set_activity(this->name, sim->now());
    }
    insert_in_server(sim->now(), arrival, amount, priority, preemptible, restart);
    status = SUCCESS;
  }
  // enqueue
  else if (room_in_queue(amount, priority)) {
    if (arrival->is_monitored())
      arrival->set_start(this->name, sim->now());
    insert_in_queue(sim->now(), arrival, amount, priority, preemptible, restart);
    status = ENQUEUED;
  }
  // reject
  else {
    arrival->reject(sim->now());
    return REJECTED;
  }
  
  if (is_monitored()) observe(sim->now());
  return status;
}

int Resource::release(Arrival* arrival, int amount) {
  // departure
  if (arrival->is_monitored()) {
    double last = arrival->get_activity(this->name);
    arrival->set_activity(this->name, sim->now() - last);
    arrival->leave(this->name, sim->now());
  }
  remove_from_server(arrival, amount);
  
  // serve another
  if (queue_count) serve_from_queue(sim->now());
  
  if (is_monitored()) observe(sim->now());
  return SUCCESS;
}
