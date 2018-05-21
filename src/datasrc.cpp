#include <simmer/process/datasrc.h>
#include <simmer/process/arrival.h>
#include <simmer/simulator.h>
#include <simmer/activity.h>

void DataSrc::run() {
  double delay = 0;
  RNum col, time = source[col_time];
  int i = 0;

  while (i++ != batch) {
    if (time.size() <= count)
      return;
    delay += time[count];

    Arrival* arrival = new_arrival(delay);

    for (size_t j = 0; j < col_attrs.size(); ++j) {
      col = source[col_attrs[j]];
      arrival->set_attribute(col_attrs[j], col[count-1]);
    }

    if (col_priority) {
      col = source[*col_priority];
      arrival->order.set_priority(col[count-1]);
    }
    if (col_preemptible) {
      col = source[*col_preemptible];
      arrival->order.set_preemptible(col[count-1]);
    }
    if (col_restart) {
      col = source[*col_restart];
      arrival->order.set_restart(col[count-1]);
    }

    // schedule the arrival
    sim->schedule(delay, arrival,
                  first_activity->priority ? first_activity->priority : count);
  }
  // schedule the generator
  sim->schedule(delay, this, priority);
}
