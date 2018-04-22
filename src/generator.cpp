#include "generator.h"
#include "simulator.h"
#include "activity.h"

void Generator::run() {
  // get the delay for the next (n) arrival(s)
  RNum delays = source();
  size_t n = delays.size();
  double delay = 0;

  for (size_t i = 0; i < n; ++i) {
    if (delays[i] < 0)
      return;
    delay += delays[i];

    // schedule the arrival
    sim->schedule(delay, new_arrival(delay),
                  first_activity->priority ? first_activity->priority : count);
  }
  // schedule the generator
  sim->schedule(delay, this, priority);
}
