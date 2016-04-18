#include "resource.h"
#include "simulator.h"

int Resource::seize(Arrival* arrival, int amount, int priority, int preemptible, bool restart) {
  int status;
  // serve now
  if (room_in_server(amount, priority)) {
    if (arrival->is_monitored()) {
      arrival->set_start(name, sim->now());
      arrival->set_activity(name, sim->now());
    }
    insert_in_server(sim->now(), arrival, amount, priority, preemptible, restart);
    status = SUCCESS;
  }
  // enqueue
  else if (room_in_queue(amount, priority)) {
    if (arrival->is_monitored())
      arrival->set_start(name, sim->now());
    insert_in_queue(sim->now(), arrival, amount, priority, preemptible, restart);
    status = ENQUEUED;
  }
  // reject
  else {
    arrival->terminate(sim->now(), false);
    return REJECTED;
  }
  
  if (is_monitored()) observe(sim->now());
  return status;
}

int Resource::release(Arrival* arrival, int amount) {
  // departure
  if (arrival->is_monitored()) {
    double last = arrival->get_activity(name);
    arrival->set_activity(name, sim->now() - last);
    arrival->leave(name, sim->now());
  }
  remove_from_server(arrival, amount);
  
  // serve another
  while (queue_count) 
    if (!try_serve_from_queue(sim->now())) break;
  
  if (is_monitored()) observe(sim->now());
  return SUCCESS;
}
