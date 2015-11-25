#include <Rcpp.h>

#include "entity.h"
#include "simulator.h"
#include "activity.h"

double Seize::run(Arrival* arrival) {
  return arrival->sim->get_resource(resource)->seize(arrival, amount);
}

double Release::run(Arrival* arrival) {
  return arrival->sim->get_resource(resource)->release(arrival, amount);
}

double Timeout::run(Arrival* arrival) {
  return fabs(Rcpp::as<double>(duration(Rcpp::wrap(*arrival->attributes))));
  
}

double SetAttribute::run(Arrival* arrival) {
  arrival->attributes->insert( std::pair<std::string, double>(key, value) );
  return SUCCESS;
}