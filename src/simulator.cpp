#include "entity.h"
#include "simulator.h"

void Simulator::notify_end(Arrival* arrival, bool finished) {
  arrival_stats->name.push_back(arrival->name);
  arrival_stats->start_time.push_back(arrival->start_time);
  arrival_stats->end_time.push_back(now_);
  arrival_stats->activity_time.push_back(arrival->activity_time);
  arrival_stats->finished.push_back(finished);
  delete arrival;
}

void Simulator::run(double until) {
  // Initialize generators if the queue is empty
  if (event_queue.empty()) {
    if (generator_vec.empty())
      throw std::runtime_error("no generators defined");
    for (GenVec::iterator itr = generator_vec.begin(); itr != generator_vec.end(); ++itr)
      (*itr)->activate();
  }
  // Loop
  while(now_ < until) {
    Event* ev = get_next();
    now_ = ev->time;
    ev->process->activate();
    delete ev;
  }
}
