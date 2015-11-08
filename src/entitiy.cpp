#include "entity.h"
#include "simulator.h"
#include "activity.h"

inline void Arrival::activate() {
  if (start_time < 0) start_time = sim->now();
  
  if (sim->verbose)
    Rcpp::Rcout <<
      "sim: " << sim->name << " | " << "time: " << sim->now() << " | " <<
      "arrival: " << name << " | " << "activity: " << 
      activity->name << "(" << activity->resource << ")" << std::endl;
  
  bool flag = TRUE;
  double delay = activity->run(this);
  if (delay == REJECTED) goto reject;
  
  activity = activity->get_next();
  if (!activity) goto finish;
  if (delay == ENQUEUED) goto end;
  
  activity_time += delay;
  sim->schedule(delay, this);
  goto end;
  
reject:
  flag = FALSE;
finish:
  sim->notify_end(this, flag);
end:
  return;
}

void Generator::activate() {
  // get the delay for the next arrival
  double delay = Rcpp::as<double>(dist());
  if (delay < 0) return;
  
  // format the name and create the next arrival
  char numstr[21];
  sprintf(numstr, "%d", count);
  Arrival* arrival = new Arrival(sim, name + numstr, is_monitored(), first_activity);
  
  // schedule this generator and the arrival
  sim->schedule(delay, this);
  sim->schedule(delay, arrival);
  
  count++;
}

int Resource::seize(Arrival* arrival, int amount) {
  // monitoring
  if (is_monitored()) observe(sim->now());
  
  // serve now
  if (room_in_server(amount)) {
    server_count += amount;
    return SUCCESS;
  }
  // enqueue
  else if (room_in_queue(amount)) {
    queue_count += amount;
    queue.push(std::make_pair(arrival, amount));
    return ENQUEUED;
  }
  // reject
  else return REJECTED;
}

int Resource::release(Arrival* arrival, int amount) {
  // monitoring
  if (is_monitored()) observe(sim->now());
  
  // departure
  server_count -= amount;
  
  // serve from the queue
  if (queue_count) {
    Arrival* another_arrival = queue.front().first;
    int another_amount = queue.front().second;
    queue.pop();
    queue_count -= another_amount;
    server_count += another_amount;
    sim->schedule(0, another_arrival);
  }
  return SUCCESS;
}
