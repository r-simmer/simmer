#include "source.h"
#include "simulator.h"
#include "arrival.h"
#include "activity.h"

Activity* trj_head(const REnv& trj) { return trj_get(trj, "head"); }

Arrival* Source::new_arrival(double delay) {
  // format the name and create the next arrival
  std::string arr_name = name + boost::lexical_cast<std::string>(count++);
  Arrival* arrival = new Arrival(sim, arr_name, is_monitored(),
                                 order, first_activity, count);

  if (sim->verbose) Rcpp::Rcout <<
    FMT(10, right) << sim->now() << " |" <<
    FMT(12, right) << "source: " << FMT(15, left) << name << "|" <<
    FMT(12, right) << "new: " << FMT(15, left) << arr_name << "| " <<
    (sim->now() + delay) << std::endl;

  return arrival;
}
