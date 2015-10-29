#include "entity.h"
#include "simulator.h"

int Resource::request(int amount) {
  // release previous resource if any
  if (sim->active_process->resource)
    sim->active_process->resource->release();
  
  if (sim->verbose)
    Rcpp::Rcout << sim->now() << ": " << sim->active_process << ": new request of " << name << std::endl;
  
  // monitoring
  if (is_monitored()) observe(sim->now());
  
  // serve now
  if (room_in_server(amount)) {
    server_count += amount;
    sim->active_process->resource = this;
    sim->active_process->amount = amount;
    sim->schedule(0, sim->active_process);
    return 0;
  }
  // enqueue
  else if (room_in_queue(amount)) {
    queue_count += amount;
    queue.push(std::make_pair(sim->active_process, amount));
    return 1;
  }
  // reject
  else {
    //sim->notify_end(arrival, 0);
    delete sim->active_process;
    sim->active_process = NULL;
    return -1;
  }
}

int Resource::release() {
  // monitoring
  if (is_monitored()) observe(sim->now());
  
  // departure
  server_count -= sim->active_process->amount;
  sim->active_process->resource = NULL;
  sim->active_process->amount = 0;
  
  // serve from the queue
  if (queue_count) {
    Process* proc = queue.front().first;
    int amount = queue.front().second;
    queue.pop();
    queue_count -= amount;
    server_count += amount;
    proc->resource = this;
    proc->amount = amount;
    sim->schedule(0, proc);
  }
  
  return 0;
}
