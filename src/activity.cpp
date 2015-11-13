#include "entity.h"
#include "simulator.h"
#include "activity.h"

double Seize::run(Arrival* arrival) {
  return arrival->sim->get_resource(resource)->seize(arrival, amount);
}

double Release::run(Arrival* arrival) {
  return arrival->sim->get_resource(resource)->release(arrival, amount);
}
