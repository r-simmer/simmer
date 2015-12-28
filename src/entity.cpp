#include "entity.h"
#include "simulator.h"
#include "activity.h"

inline void Arrival::activate() {
  double delay;
  bool flag = TRUE;
  if (!activity) goto finish;
  
  if (lifetime.start < 0) lifetime.start = sim->now();
  
  if (sim->verbose)
    Rcpp::Rcout <<
      "sim: " << sim->name << " | " << "time: " << sim->now() << " | " <<
      "arrival: " << name << " | " << "activity: " << 
      activity->name << "(" << activity->resource << ")" << std::endl;
  
  delay = activity->run(this);
  if (delay == REJECTED) goto reject;
  activity = activity->get_next();
  if (delay == ENQUEUED) goto end;
  
  lifetime.activity += delay;
  sim->schedule(delay, this, activity ? activity->priority : 0);
  goto end;
  
reject:
  flag = FALSE;
finish:
  gen->notify_end(sim->now(), this, flag);
end:
  return;
}

inline void Arrival::leaving(std::string name, double time) {
  gen->notify_release(time, this, name);
}

void Generator::activate() {
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
  
  // monitoring
  if (is_monitored() >= 2) gen->observe(sim->now(), this, key);
  
  return 0;
}

int Resource::seize(Arrival* arrival, int amount, int priority) {
  // monitoring
  if (is_monitored()) observe(sim->now());
  
  // serve now
  if (room_in_server(amount)) {
    if (arrival->is_monitored()) {
      arrival->set_start(this->name, sim->now());
      arrival->set_activity(this->name, sim->now());
    }
    server_count += amount;
    return SUCCESS;
  }
  // enqueue
  if (room_in_queue(amount)) {
    if (arrival->is_monitored())
      arrival->set_start(this->name, sim->now());
    queue_count += amount;
    queue.push(RQItem(arrival, amount, priority, sim->now()));
    return ENQUEUED;
  }
  // reject
  return REJECTED;
}

int Resource::release(Arrival* arrival, int amount) {
  // monitoring
  if (is_monitored()) observe(sim->now());
  if (arrival->is_monitored()) {
    double last = arrival->get_activity(this->name);
    arrival->set_activity(this->name, sim->now() - last);
    arrival->leaving(this->name, sim->now());
  }
  
  // departure
  server_count -= amount;
  
  // serve from the queue
  if (queue_count) {
    queue_count -= queue.top().amount;
    server_count += queue.top().amount;
    sim->schedule(0, queue.top().arrival);
    if (queue.top().arrival->is_monitored())
      queue.top().arrival->set_activity(this->name, sim->now());
    queue.pop();
  }
  return SUCCESS;
}
