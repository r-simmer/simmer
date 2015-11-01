#include "entity.h"
#include "simulator.h"

inline void Arrival::activate() {
  if (start_time < 0) start_time = sim->now();
  
  if (sim->verbose)
    Rcpp::Rcout <<
      "sim: " << sim->name << " | " << "time: " << sim->now() << " | " <<
      "arrival: " << name << " | " << "activity: " << 
      Rcpp::as<std::string>(activity["name"]) << "(" <<
      Rcpp::as<std::string>(activity["resource"]) << ")" << std::endl;
  
  // run the activity and get the activity time
  Rcpp::Function run(activity["run"]);
  double delay = Rcpp::as<double>(run((long int)this));
  if (delay >= 0) activity_time += delay;
  
  // get the next activity or end
  if (delay >= -1) {
    if (Rf_isEnvironment(activity["next_activity"])) {
      activity = activity["next_activity"];
      if (delay >= 0) sim->schedule(delay, this);
    } else sim->notify_end(this, TRUE);
  } else sim->notify_end(this, FALSE);
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
    return 0;
  }
  // enqueue
  else if (room_in_queue(amount)) {
    queue_count += amount;
    queue.push(std::make_pair(arrival, amount));
    return -1;
  }
  // reject
  else return -2;
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
  
  return 0;
}
