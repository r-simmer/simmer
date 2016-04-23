#include "process.h"
#include "simulator.h"
#include "activity.h"

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

void Manager::run() {
  if (!is_active()) goto end;
  
  if (sim->now() || !duration[index]) {
    set(value[index]);
    index++;
    if (index == duration.size()) {
      if (period < 0) goto end;
      index = 1;
    }
  }
  sim->schedule(duration[index], this, -10);
end:
  return;
}

void Arrival::run() {
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
  terminate(sim->now(), true);
end:
  return;
}

void Arrival::activate() {
  Process::activate();
  busy_until = sim->now() + remaining;
  sim->schedule(remaining, this, 1);
  remaining = 0;
}

void Arrival::deactivate(bool restart) {
  Process::deactivate(restart);
  remaining = busy_until - sim->now();
  if (remaining && restart) {
    lifetime.activity -= remaining;
    remaining = 0;
    activity = activity->get_prev();
  }
}

int Arrival::set_attribute(std::string key, double value) {
  attributes[key] = value;
  if (is_monitored() >= 2) 
    gen->observe(sim->now(), name, key, value);
  return 0;
}
