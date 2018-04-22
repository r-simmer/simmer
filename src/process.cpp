#include "process.h"
#include "simulator.h"

bool Process::activate(double delay) {
  sim->schedule(delay, this, priority);
  return true;
}

bool Process::deactivate() {
  if (!sim->is_scheduled(this))
    return false;
  sim->unschedule(this);
  return true;
}
