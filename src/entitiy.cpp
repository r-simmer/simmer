#include "entity.h"
#include "simulator.h"

int Resource::request(int amount) {
  // monitoring
  if (is_monitored()) observe(sim->now());
  
  // serve now
  if (room_in_server(amount)) {
    server_count += amount;
    server[sim->active_process] = amount;
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
    return -1;
  }
}

int Resource::release() {
  // monitoring
  if (is_monitored()) observe(sim->now());
  
  // departure
  ServerMap::const_iterator got = server.find(sim->active_process);
  if (got == server.end()) return -1; // not found
  server_count -= got->second;
  
  // serve from the queue
  if (queue_count) {
    Rcpp::Function process = queue.front().first;
    int amount = queue.front().second;
    queue.pop();
    queue_count -= amount;
    server_count += amount;
    server[process] = amount;
    sim->process(process);
  }
  
  return 0;
}
