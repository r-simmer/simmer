#include "simulator.h"
#include "resource.h"

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
  if (is_monitored())
    sim->record_resource(name, server_count, queue_count, capacity, queue_size);
}

void Resource::set_queue_size(int value) {
  if (queue_size == value) return;
  queue_size = value;
  if (is_monitored()) 
    sim->record_resource(name, server_count, queue_count, capacity, queue_size);
}

int Resource::seize(Arrival* arrival, int amount) {
  int status;
  // serve now
  if (room_in_server(amount, arrival->order.get_priority())) {
    if (arrival->is_monitored()) {
      arrival->set_start(this->name, sim->now());
      arrival->set_activity(this->name, sim->now());
    }
    insert_in_server(sim->verbose, sim->now(), arrival, amount);
    status = SUCCESS;
  }
  // enqueue
  else if (room_in_queue(amount, arrival->order.get_priority())) {
    if (arrival->is_monitored()) {
      arrival->set_start(this->name, sim->now());
      arrival->set_activity(this->name, 0);
    }
    insert_in_queue(sim->verbose, sim->now(), arrival, amount);
    status = ENQUEUED;
  }
  // reject
  else {
    if (sim->verbose) verbose_print(sim->now(), arrival->name, "REJECT");
    return REJECTED;
  }
  
  arrival->register_entity(this);
  if (is_monitored()) 
    sim->record_resource(name, server_count, queue_count, capacity, queue_size);
  return status;
}

int Resource::release(Arrival* arrival, int amount) {
  // departure
  if (arrival->is_monitored()) {
    double last = arrival->get_activity(this->name);
    arrival->set_activity(this->name, sim->now() - last);
    arrival->leave(this->name);
  }
  remove_from_server(sim->verbose, sim->now(), arrival, amount);
  arrival->unregister_entity(this);
  
  // serve another
  Task* task = new Task(sim, "Post-Release", boost::bind(&Resource::post_release, this));
  sim->schedule(0, task, PRIORITY_RELEASE_POST);
  
  return SUCCESS;
}

int Resource::post_release() {
  // serve another
  while (queue_count) 
    if (!try_serve_from_queue(sim->verbose, sim->now())) break;
    
  if (is_monitored())
    sim->record_resource(name, server_count, queue_count, capacity, queue_size);
  return SUCCESS;
}

bool Resource::erase(Arrival* arrival, bool stay) {
  if (stay) {
    int amount = remove_from_server(false, sim->now(), arrival, -1);
    server_count += amount;
    return false;
  }
  
  if (!remove_from_queue(sim->verbose, sim->now(), arrival)) {
    release(arrival, -1);
    return false;
  }
  
  if (is_monitored())
    sim->record_resource(name, server_count, queue_count, capacity, queue_size);
  return true;
}
