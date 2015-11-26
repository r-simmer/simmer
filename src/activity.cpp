#include <Rcpp.h>

#include "entity.h"
#include "simulator.h"
#include "activity.h"

double Seize::run(Arrival* arrival) {
  if(provide_attrs)
  {
    return arrival->sim->get_resource(resource)->seize(arrival, Rcpp::as<int>(amount(Rcpp::wrap(*arrival->attributes))));  
  }
  else 
  {
    return arrival->sim->get_resource(resource)->seize(arrival, Rcpp::as<int>(amount()));
  }
  
}

double Release::run(Arrival* arrival) {
  if(provide_attrs)
  {
    return arrival->sim->get_resource(resource)->release(arrival, Rcpp::as<int>(amount(Rcpp::wrap(*arrival->attributes))));  
  }
  else 
  {
    return arrival->sim->get_resource(resource)->release(arrival, Rcpp::as<int>(amount()));
  }
}

double Timeout::run(Arrival* arrival) {
  
  if(provide_attrs)
  {
    return fabs(Rcpp::as<double>(task(Rcpp::wrap(*arrival->attributes))));  
  }
  else 
  {
    return fabs(Rcpp::as<double>(task()));  
  }
  
}

double SetAttribute::run(Arrival* arrival) {
  if(provide_attrs)
  {
    arrival->attributes->operator[](key) = Rcpp::as<double>(value(Rcpp::wrap(*arrival->attributes)));
  }
  else 
  {
    arrival->attributes->operator[](key) = Rcpp::as<double>(value());
  }
  
  
  return SUCCESS;
}