#include "entity.h"
#include "simulator.h"
#include "activity.h"

template <typename T>
T Activity::execute_call(Rcpp::Function call, Arrival* arrival) {
  if (provide_attrs)
    return Rcpp::as<T>(call(Rcpp::wrap(*arrival->get_attributes())));
  else return Rcpp::as<T>(call());
}

template <>
double Seize<int>::run(Arrival* arrival) {
  return arrival->sim->get_resource(resource)->seize(arrival, amount);
}

template <>
double Seize<Rcpp::Function>::run(Arrival* arrival) {
  int ret = execute_call<int>(amount, arrival);
  return arrival->sim->get_resource(resource)->seize(arrival, ret);
}

template <>
double SeizeSelected<int>::run(Arrival* arrival) {
  return arrival->get_selected(id)->seize(arrival, amount);
}

template <>
double SeizeSelected<Rcpp::Function>::run(Arrival* arrival) {
  int ret = execute_call<int>(amount, arrival);
  return arrival->get_selected(id)->seize(arrival, ret);
}

template <>
double Release<int>::run(Arrival* arrival) {
  return arrival->sim->get_resource(resource)->release(arrival, amount);
}

template <>
double Release<Rcpp::Function>::run(Arrival* arrival) {
  int ret = execute_call<int>(amount, arrival);
  return arrival->sim->get_resource(resource)->release(arrival, ret);
}

template <>
double ReleaseSelected<int>::run(Arrival* arrival) {
  return arrival->get_selected(id)->release(arrival, amount);
}

template <>
double ReleaseSelected<Rcpp::Function>::run(Arrival* arrival) {
  int ret = execute_call<int>(amount, arrival);
  return arrival->get_selected(id)->release(arrival, ret);
}

template <>
double Timeout<double>::run(Arrival* arrival) { return std::abs(delay); }

template <>
double Timeout<Rcpp::Function>::run(Arrival* arrival) {
  double ret = execute_call<double>(delay, arrival);
  return std::abs(ret);
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

double Branch::run(Arrival* arrival) {
  unsigned int ret = execute_call<unsigned int>(option, arrival);
  if (ret < 1 || ret > heads.size())
    Rcpp::stop("index out of range");
  selected = heads[ret-1];
  return 0;
}

template <>
void Rollback<int>::print(int indent, bool brief) {
  if (!cached) cached = goback();
  Activity::print(indent, brief);
  if (!brief) {
    Rcpp::Rcout << "amount: " << amount << " (" << cached->name << "), ";
    if (times >= 0) Rcpp::Rcout << "times: " << times << " }" << std::endl;
    else Rcpp::Rcout << "times: Inf }" << std::endl;
  } else Rcpp::Rcout << cached->name << std::endl;
}

template <>
void Rollback<Rcpp::Function>::print(int indent, bool brief) {
  if (!cached) cached = goback();
  Activity::print(indent, brief);
  if (!brief) Rcpp::Rcout << 
    "amount: " << amount << " (" << cached->name << "), " << 
    "check: " << times << " }" << std::endl;
  else Rcpp::Rcout << cached->name << std::endl;
}

template <>
double Rollback<int>::run(Arrival* arrival) {
  if (times >= 0) {
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

template <>
double Rollback<Rcpp::Function>::run(Arrival* arrival) {
  if (!execute_call<bool>(times, arrival)) return 0;
  if (!cached) cached = goback();
  selected = cached;
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
    arrival->terminate(arrival->sim->now(), false);
    return REJECTED;
  }
  return 0;
}

template <>
double Leave<Rcpp::Function>::run(Arrival* arrival) {
  Rcpp::NumericVector val = Rcpp::runif(1);
  if (val[0] <= execute_call<double>(prob, arrival)) {
    arrival->terminate(arrival->sim->now(), false);
    return REJECTED;
  }
  return 0;
}
