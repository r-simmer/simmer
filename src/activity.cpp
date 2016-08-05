#include "activity.h"

template <>
double Seize<int>::run(Arrival* arrival) {
  int ret = arrival->sim->get_resource(resource)->seize(arrival, amount);
  return select(arrival, ret);
}

template <>
double Seize<Rcpp::Function>::run(Arrival* arrival) {
  int value = execute_call<int>(amount, arrival);
  int ret = arrival->sim->get_resource(resource)->seize(arrival, value);
  return select(arrival, ret);
}

template <>
double SeizeSelected<int>::run(Arrival* arrival) {
  int ret = arrival->get_selected(id)->seize(arrival, amount);
  return select(arrival, ret);
}

template <>
double SeizeSelected<Rcpp::Function>::run(Arrival* arrival) {
  int value = execute_call<int>(amount, arrival);
  int ret = arrival->get_selected(id)->seize(arrival, value);
  return select(arrival, ret);
}

template <>
double Release<int>::run(Arrival* arrival) {
  return arrival->sim->get_resource(resource)->release(arrival, amount);
}

template <>
double Release<Rcpp::Function>::run(Arrival* arrival) {
  int value = execute_call<int>(amount, arrival);
  return arrival->sim->get_resource(resource)->release(arrival, value);
}

template <>
double ReleaseSelected<int>::run(Arrival* arrival) {
  return arrival->get_selected(id)->release(arrival, amount);
}

template <>
double ReleaseSelected<Rcpp::Function>::run(Arrival* arrival) {
  int value = execute_call<int>(amount, arrival);
  return arrival->get_selected(id)->release(arrival, value);
}

template <>
double Timeout<double>::run(Arrival* arrival) { return std::abs(delay); }

template <>
double Timeout<Rcpp::Function>::run(Arrival* arrival) {
  double value = execute_call<double>(delay, arrival);
  return std::abs(value);
}

template <>
double SetAttribute<double>::run(Arrival* arrival) {
  return arrival->set_attribute(key, value);
}

template <>
double SetAttribute<Rcpp::Function>::run(Arrival* arrival) {
  double ret = execute_call<double>(value, arrival);
  return arrival->set_attribute(key, ret);
}

template <>
void SetPrior<VEC<int> >::print(int indent, bool brief) {
  if (values.size() != 3) Rcpp::stop("%s: 3 values needed", name);
  Activity::print(indent, brief);
  if (!brief) Rcpp::Rcout << "values: " << 
    values[0] << " " << values[1] << " " << values[2] << " }" << std::endl;
  else Rcpp::Rcout << values[0] << " " << values[1] << " " << values[2] << std::endl;
}

template <>
void SetPrior<Rcpp::Function>::print(int indent, bool brief) {
  Activity::print(indent, brief);
  if (!brief) Rcpp::Rcout << "values: " << values << " }" << std::endl;
  else Rcpp::Rcout << values << std::endl;
}

template <>
double SetPrior<VEC<int> >::run(Arrival* arrival) {
  if (values.size() != 3) Rcpp::stop("%s: 3 values needed", name);
  if (values[0] >= 0) arrival->order.set_priority(values[0]);
  if (values[1] >= 0) arrival->order.set_preemptible(values[1]);
  if (values[2] >= 0) arrival->order.set_restart((bool)values[2]);
  return 0;
}

template <>
double SetPrior<Rcpp::Function>::run(Arrival* arrival) {
  VEC<int> ret = execute_call<VEC<int> >(values, arrival);
  if (ret.size() != 3) Rcpp::stop("%s: 3 values needed", name);
  if (ret[0] >= 0) arrival->order.set_priority(ret[0]);
  if (ret[1] >= 0) arrival->order.set_preemptible(ret[1]);
  if (ret[2] >= 0) arrival->order.set_restart((bool)ret[2]);
  return 0;
}

template <>
void Select<VEC<std::string> >::print(int indent, bool brief) {
  Activity::print(indent, brief);
  if (!brief) Rcpp::Rcout << 
    "resources: " << resources[0] << (resources.size()>1 ? ", ..." : "") << " | " << 
      "policy: " << policy << " }" << std::endl;
  else {
    if (resources.size() > 1)
      Rcpp::Rcout << resources.size() << " options" << std::endl;
    else Rcpp::Rcout << resources[0] << std::endl;
  }
}

template <>
void Select<Rcpp::Function>::print(int indent, bool brief) {
  Activity::print(indent, brief);
  if (!brief) Rcpp::Rcout << "resources: " << resources << " }" << std::endl;
  else Rcpp::Rcout << resources << std::endl;
}

template <>
double Select<VEC<std::string> >::run(Arrival* arrival) {
  Resource* selected;
  if (resources.size() == 1) 
    selected = arrival->sim->get_resource(resources[0]);
  else selected = dispatcher.dispatch(arrival->sim);
  arrival->set_selected(id, selected);
  return 0;
}

template <>
double Select<Rcpp::Function>::run(Arrival* arrival) {
  std::string res = execute_call<std::string>(resources, arrival);
  Resource* selected = arrival->sim->get_resource(res);
  arrival->set_selected(id, selected);
  return 0;
}

template <>
double Leave<double>::run(Arrival* arrival) {
  Rcpp::NumericVector val = Rcpp::runif(1);
  if (val[0] <= prob) {
    arrival->terminate(false);
    return REJECTED;
  }
  return 0;
}

template <>
double Leave<Rcpp::Function>::run(Arrival* arrival) {
  Rcpp::NumericVector val = Rcpp::runif(1);
  if (val[0] <= execute_call<double>(prob, arrival)) {
    arrival->terminate(false);
    return REJECTED;
  }
  return 0;
}

template <>
double Clone<int>::run(Arrival* arrival) {
  do_clone(arrival, n);
  return 0;
}

template <>
double Clone<Rcpp::Function>::run(Arrival* arrival) {
  int value = std::abs(execute_call<int>(n, arrival));
  do_clone(arrival, value);
  return 0;
}

template <>
double RenegeIn<double>::run(Arrival* arrival) {
  return 0;
}

template <>
double RenegeIn<Rcpp::Function>::run(Arrival* arrival) {
  double ret = execute_call<double>(t, arrival);
  return 0;
}
