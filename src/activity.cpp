#include <Rcpp.h>

#include "entity.h"
#include "simulator.h"
#include "activity.h"

template <class T>
inline T execute_call(Rcpp::Function call, Arrival* arrival, bool provide_attrs) {
  if (provide_attrs)
    return Rcpp::as<T>(call(Rcpp::wrap(*arrival->get_attributes())));
  else return Rcpp::as<T>(call());
}

double Seize::run(Arrival* arrival) {
  if (has_func)
    amount = execute_call<int>(amount_func, arrival, provide_attrs);
  return arrival->sim->get_resource(resource)->seize(arrival, amount);
}

double Release::run(Arrival* arrival) {
  if (has_func)
    amount = execute_call<int>(amount_func, arrival, provide_attrs);
  return arrival->sim->get_resource(resource)->release(arrival, amount);
}

double Timeout::run(Arrival* arrival) {
  if (has_func)
    delay = execute_call<double>(task, arrival, provide_attrs);
  return fabs(delay);
}

double SetAttribute::run(Arrival* arrival) {
  if (has_func)
    value = execute_call<double>(value_func, arrival, provide_attrs);
  return arrival->set_attribute(key, value);
}

double Rollback::run(Arrival* arrival) {
  if (has_func) {
    if (!execute_call<bool>(check, arrival, provide_attrs))return 0;
  } else if (times >= 0) {
    if (pending.find(arrival) == pending.end()) 
      pending[arrival] = times;
    if (!pending[arrival]) {
      pending.erase(arrival);
      return 0;
    }
    pending[arrival]--;
  }
  
  if (!cached) cached = goback();
  selected = cached;
  return 0;
}
