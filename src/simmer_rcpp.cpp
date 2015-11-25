#include <Rcpp.h>

#include "activity.h"
#include "entity.h"
#include "simulator.h"

using namespace Rcpp;

//[[Rcpp::export]]
SEXP Simulator__new(SEXP name_, SEXP verbose_) {
  std::string name = as<std::string>(name_);
  bool verbose = as<bool>(verbose_);
  
  XPtr<Simulator> ptr(new Simulator(name, verbose));
  return ptr;
}

//[[Rcpp::export]]
void reset_(SEXP sim_) {
  XPtr<Simulator> sim(sim_);
  
  sim->reset();
}

//[[Rcpp::export]]
double now_(SEXP sim_) {
  XPtr<Simulator> sim(sim_);
  
  return sim->now();
}

//[[Rcpp::export]]
double peek_(SEXP sim_) {
  XPtr<Simulator> sim(sim_);
  
  return sim->peek();
}

//[[Rcpp::export]]
void step_(SEXP sim_) {
  XPtr<Simulator> sim(sim_);
  
  sim->step();
}

//[[Rcpp::export]]
void run_(SEXP sim_, SEXP until_) {
  XPtr<Simulator> sim(sim_);
  double until = as<double>(until_);
  
  sim->run(until);
}

//[[Rcpp::export]]
void add_generator_(SEXP sim_, SEXP name_prefix_, SEXP first_activity_, SEXP dist_, SEXP mon_) {
  XPtr<Simulator> sim(sim_);
  std::string name_prefix = as<std::string>(name_prefix_);
  XPtr<Activity> first_activity(first_activity_);
  Function dist(dist_);
  bool mon = as<bool>(mon_);
  
  sim->add_generator(name_prefix, first_activity, dist, mon);
}

//[[Rcpp::export]]
void add_resource_(SEXP sim_, SEXP name_, SEXP capacity_, SEXP queue_size_, SEXP mon_) {
  XPtr<Simulator> sim(sim_);
  std::string name = as<std::string>(name_);
  int capacity = as<int>(capacity_);
  int queue_size = as<int>(queue_size_);
  bool mon = as<bool>(mon_);
  
  sim->add_resource(name, capacity, queue_size, mon);
}

//[[Rcpp::export]]
SEXP get_mon_arrivals_(SEXP sim_) {
  XPtr<Simulator> sim(sim_);
  
  ArrStats* stats = sim->get_mon_arrivals();
  
  return Rcpp::List::create(Rcpp::Named("name") = stats->name,
                            Rcpp::Named("start_time") = stats->start_time,
                            Rcpp::Named("end_time") = stats->end_time,
                            Rcpp::Named("activity_time") = stats->activity_time,
                            Rcpp::Named("finished") = stats->finished);
}

//[[Rcpp::export]]
SEXP get_mon_resource_(SEXP sim_, SEXP name_) {
  XPtr<Simulator> sim(sim_);
  std::string name = as<std::string>(name_);
  
  ResStats* stats = sim->get_resource(name)->get_observations();
  
  return Rcpp::List::create(Rcpp::Named("time") = stats->time,
                            Rcpp::Named("server") = stats->server,
                            Rcpp::Named("queue") = stats->queue);
}

//[[Rcpp::export]]
int get_capacity_(SEXP sim_, SEXP name_){
  XPtr<Simulator> sim(sim_);
  std::string name = as<std::string>(name_);
  
  return sim->get_resource(name)->get_capacity();
}

//[[Rcpp::export]]
int get_queue_size_(SEXP sim_, SEXP name_){
  XPtr<Simulator> sim(sim_);
  std::string name = as<std::string>(name_);
  
  return sim->get_resource(name)->get_queue_size();
}

//[[Rcpp::export]]
int get_server_count_(SEXP sim_, SEXP name_){
  XPtr<Simulator> sim(sim_);
  std::string name = as<std::string>(name_);
  
  return sim->get_resource(name)->get_server_count();
}

//[[Rcpp::export]]
int get_queue_count_(SEXP sim_, SEXP name_){
  XPtr<Simulator> sim(sim_);
  std::string name = as<std::string>(name_);
  
  return sim->get_resource(name)->get_queue_count();
}

//[[Rcpp::export]]
SEXP Seize__new(SEXP resource_, SEXP amount_) {
  std::string resource = as<std::string>(resource_);
  int amount = as<int>(amount_);
  
  XPtr<Seize> ptr(new Seize(resource, amount), false);
  return ptr;
}

//[[Rcpp::export]]
SEXP Release__new(SEXP resource_, SEXP amount_) {
  std::string resource = as<std::string>(resource_);
  int amount = as<int>(amount_);
  
  XPtr<Release> ptr(new Release(resource, amount), false);
  return ptr;
}

//[[Rcpp::export]]
SEXP SetAttribute__new(SEXP key_, SEXP value_) {
  std::string key = as<std::string>(key_);
  double value = as<double>(value_);
  
  XPtr<SetAttribute> ptr(new SetAttribute(key, value), false);
  return ptr;
}

//[[Rcpp::export]]
SEXP Timeout__new(Function duration) {
  XPtr<Timeout> ptr(new Timeout(duration), false);
  return ptr;
}

//[[Rcpp::export]]
SEXP Branch__new(Function option, SEXP merge_, SEXP trj_) {
  std::vector<bool> merge = as<std::vector<bool> >(merge_);
  std::vector<Environment> trj = as<std::vector<Environment> >(trj_);
  
  XPtr<Branch> ptr(new Branch(option, merge, trj), false);
  return ptr;
}

//[[Rcpp::export]]
SEXP Rollback__new(SEXP amount_, SEXP times_) {
  int amount = as<int>(amount_);
  int times = as<int>(times_);
  
  XPtr<Rollback> ptr(new Rollback(amount, times), false);
  return ptr;
}

//[[Rcpp::export]]
int activity_get_n_(SEXP activity_) {
  XPtr<Activity> activity(activity_);
  
  return activity->n;
}

//[[Rcpp::export]]
void activity_show_(SEXP activity_, SEXP indent_) {
  XPtr<Activity> activity(activity_);
  int indent = as<int>(indent_);
  
  return activity->show(indent);
}

//[[Rcpp::export]]
SEXP activity_get_next_(SEXP activity_) {
  XPtr<Activity> activity(activity_);
  
  Activity* the_next = activity->get_next();
  if (the_next) {
    XPtr<Activity> ptr(the_next, false);
    return ptr;
  } else return R_NilValue;
}

//[[Rcpp::export]]
SEXP activity_get_prev_(SEXP activity_) {
  XPtr<Activity> activity(activity_);
  
  Activity* the_prev = activity->get_prev();
  if (the_prev) {
    XPtr<Activity> ptr(the_prev, false);
    return ptr;
  } else return R_NilValue;
}

//[[Rcpp::export]]
void activity_chain_(SEXP activity_, SEXP the_next_) {
  XPtr<Activity> activity(activity_);
  XPtr<Activity> the_next(the_next_);
  
  activity->set_next(the_next);
  the_next->set_prev(activity);
}
