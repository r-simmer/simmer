#include "process.h"
#include "simulator.h"
#include "activity.h"

void Process::deactivate() { sim->unschedule(this); }

void Generator::run() {
  // get the delay for the next (n) arrival(s)
  Rcpp::NumericVector delays = dist();
  int n = delays.size();
  double delay = 0;
  
  for(int i = 0; i < n; ++i) {
    if (delays[i] < 0) return;
    delay += delays[i];
    
    // format the name and create the next arrival
    char numstr[21];
    sprintf(numstr, "%d", count);
    Arrival* arrival = new Arrival(sim, name + numstr, is_monitored(), order, first_activity);
    
    if (sim->verbose) Rcpp::Rcout << 
      FMT(10, right) << sim->now() << " |" << FMT(12, right) << "generator: " << FMT(15, left) << name << "|" << 
      FMT(12, right) << "new: " << FMT(15, left) << (name + numstr) << "| " << (sim->now() + delay) << std::endl;
    
    // schedule the arrival
    sim->schedule(delay, arrival, count);
    count++;
  }
  // schedule the generator
  sim->schedule(delay, this, PRIORITY_GENERATOR);
}

void Manager::run() {
  if (!sim->now() && duration[index]) goto finish;
  if (sim->verbose) Rcpp::Rcout <<
    FMT(10, right) << sim->now() << " |" << FMT(12, right) << "manager: " << FMT(15, left) << name << "|" << 
    FMT(12, right) << "parameter: " << FMT(15, left) << param << "| " << value[index] << std::endl;
  
  set(value[index]);
  index++;
  if (index == duration.size()) {
    if (period < 0) goto end;
    index = 1;
  }
  
finish:
  sim->schedule(duration[index], this, PRIORITY_MANAGER);
end:
  return;
}

void DelayedTask::run() {
  if (sim->verbose) Rcpp::Rcout <<
    FMT(10, right) << sim->now() << " |" << FMT(12, right) << "task: " << FMT(15, left) << name << "|" << 
    FMT(12, right) << " " << FMT(15, left) << " " << "| " << std::endl;
  
  task();
  delete this;
}

void Arrival::run() {
  double delay;
  
  if (!activity) goto finish;
  if (lifetime.start < 0) lifetime.start = sim->now();
  
  if (sim->verbose) {
    Rcpp::Rcout <<
      FMT(10, right) << sim->now() << " |" << 
      FMT(12, right) << "arrival: " << FMT(15, left) << name << "|" << 
      FMT(12, right) << "activity: " << FMT(15, left) << activity->name << "| ";
    activity->print(0, true);
  }
  
  delay = activity->run(this);
  if (delay == REJECTED) goto end;
  activity = activity->get_next();
  if (delay == ENQUEUED) goto end;
  
  lifetime.busy_until = sim->now() + delay;
  lifetime.activity += delay;
  sim->schedule(delay, this, activity ? activity->priority : 0);
  goto end;
  
finish:
  terminate(true);
end:
  return;
}

void Arrival::forward_activity() { activity = activity->get_next(); }

void Arrival::activate() {
  Process::activate();
  lifetime.busy_until = sim->now() + lifetime.remaining;
  sim->schedule(lifetime.remaining, this, 1);
  lifetime.remaining = 0;
}

void Arrival::deactivate() {
  Process::deactivate();
  lifetime.remaining = lifetime.busy_until - sim->now();
  if (lifetime.remaining && order.get_restart()) {
    lifetime.activity -= lifetime.remaining;
    lifetime.remaining = 0;
    activity = activity->get_prev();
  }
}

void Arrival::leave(std::string resource) {
  sim->record_release(name, restime[resource].start, restime[resource].activity, resource);
}

void Arrival::terminate(bool finished) {
  lifetime.activity -= lifetime.remaining;
  if (is_monitored() >= 1)
    sim->record_end(name, lifetime.start, lifetime.activity, finished);
  delete this;
}

int Arrival::set_attribute(std::string key, double value) {
  attributes[key] = value;
  if (is_monitored() >= 2) 
    sim->record_attribute(name, key, value);
  return 0;
}
