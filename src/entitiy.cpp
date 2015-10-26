#include "entity.h"
#include "simulator.h"

inline void Arrival::activate() {
  if (start_time < 0) start_time = sim->now();
  
  Rcpp::Environment current_event = event;
  if (current_event == NULL)
    sim->notify_end(this, 1);
  else {
    if (sim->verbose)
      Rcpp::Rcout <<
        "rep: " << sim->n << " | " << "time: " << sim->now() << " | " <<
        "arrival: " << name << " | " << "event: " << 
        Rcpp::as<std::string>(current_event["name"]) << "(" <<
        Rcpp::as<std::string>(current_event["resource"]) << ")" << std::endl;
    event = event["next_event"];
    Rcpp::Function run(current_event["run"]);
    activity_time += Rcpp::as<double>(run((long int)this));
    
  }
}

void Generator::activate() {
  double delay = fabs(Rcpp::as<double>(dist()));
  char numstr[21];
  sprintf(numstr, "%d", count);
  Arrival* arrival = new Arrival(sim, name + numstr, first_event);
  sim->schedule(delay, this);
  sim->schedule(delay, arrival);
  count++;
}

int Resource::seize(Arrival* arrival, int amount) {
  if (mon) observe(sim->now());
  
  if (room_in_server(amount)) {
    server_count += amount;
    return 0; // serving now
  } else if (room_in_queue(amount)) {
    queue_count += amount;
    queue.push(std::make_pair(arrival, amount));
    return 1; // enqueued
  } else {
    sim->notify_end(arrival, 0);
    return -1; // reject
  }
}

int Resource::release(Arrival* arrival, int amount) {
  if (mon) observe(sim->now());
  
  // departure
  server_count -= amount;
  
  // serve from the queue
  if (queue_count) {
    Arrival* another_arrival = queue.front().first;
    int another_amount = queue.front().second;
    queue_count -= another_amount;
    server_count += another_amount;
    sim->schedule(0, another_arrival);
  }
  return 0;
}
