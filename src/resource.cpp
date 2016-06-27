#include "resource.h"
#include "simulator.h"

void Resource::set_capacity(int value) {
  if (capacity == value) return;
  int last = capacity;
  capacity = value;
  if (capacity > last || capacity < 0) {
    // serve another
    while (queue_count) 
      if (!try_serve_from_queue(sim->verbose, sim->now())) break;
  } else if (capacity < last) {
    while (server_count > capacity) 
      if (!try_free_server(sim->verbose, sim->now())) break;
  }
  if (is_monitored()) observe(sim->now());
}

void Resource::set_queue_size(int value) {
  if (queue_size == value) return;
  queue_size = value;
  if (is_monitored()) observe(sim->now());
}

int Resource::seize(Arrival* arrival, int amount, int priority, int preemptible, bool restart) {
  int status;
  // serve now
  if (room_in_server(amount, priority)) {
    if (arrival->is_monitored()) {
      arrival->set_start(this->name, sim->now());
      arrival->set_activity(this->name, sim->now());
    }
    insert_in_server(sim->verbose, sim->now(), arrival, amount, priority, preemptible, restart);
    status = SUCCESS;
  }
  // enqueue
  else if (room_in_queue(amount, priority)) {
    if (arrival->is_monitored()) {
      arrival->set_start(this->name, sim->now());
      arrival->set_activity(this->name, 0);
    }
    insert_in_queue(sim->verbose, sim->now(), arrival, amount, priority, preemptible, restart);
    status = ENQUEUED;
  }
  // reject
  else {
    if (sim->verbose) verbose_print(sim->now(), arrival->name, "REJECT");
    arrival->terminate(sim->now(), false);
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
  remove_from_server(sim->verbose, sim->now(), arrival, amount);
  
  // serve another
  DelayedTask* task = new DelayedTask(sim, "Post-Release", 
                                      boost::bind(&Resource::post_release, this));
  sim->schedule(0, task, PRIORITY_RELEASE_POST);
  
  return SUCCESS;
}

int Resource::post_release() {
  // serve another
  while (queue_count) 
    if (!try_serve_from_queue(sim->verbose, sim->now())) break;
    
  if (is_monitored()) observe(sim->now());
  return SUCCESS;
}
