#include "entity.h"
#include "simulator.h"

// void Simulator::notify_end(Arrival* arrival, bool finished) {
//   if (arrival->is_monitored()) {
//     arrival_stats->name.push_back(arrival->name);
//     arrival_stats->start_time.push_back(arrival->start_time);
//     arrival_stats->end_time.push_back(now_);
//     arrival_stats->activity_time.push_back(arrival->activity_time);
//     arrival_stats->finished.push_back(finished);
//   }
//   delete arrival;
// }

void Simulator::run(double until) {
  // Loop
  while(now_ < until) {
    Event* ev = get_next();
    now_ = ev->time;
    active_process = ev->process;
    delete ev;
    int ret = Rcpp::as<int>(active_process());
  }
  // ... and that's it! :D
}
