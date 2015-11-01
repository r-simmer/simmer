#include <Rcpp.h>

#include "entity.h"
#include "simulator.h"

using namespace Rcpp;

//[[Rcpp::export]]
SEXP Simulator__new(SEXP name_, SEXP verbose_) {
  std::string name = as<std::string>(name_);
  bool verbose = as<bool>(verbose_);
  
  XPtr<Simulator> ptr(new Simulator(name, verbose), false); // What does this "false" mean?
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

// //[[Rcpp::export]]
// void schedule_(SEXP delay_, SEXP arrival_) {
//   double delay = as<double>(delay_);
//   Arrival* arrival = (Arrival*)as<long int>(arrival_);
//   
//   arrival->sim->schedule(delay, arrival);
// }

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
void resource_(SEXP sim_, SEXP name_, SEXP capacity_, SEXP queue_size_, SEXP mon_) {
  XPtr<Simulator> sim(sim_);
  std::string name = as<std::string>(name_);
  int capacity = as<int>(capacity_);
  int queue_size = as<int>(queue_size_);
  bool mon = as<bool>(mon_);
  
  sim->resource(name, capacity, queue_size, mon);
}

//[[Rcpp::export]]
void process_(SEXP sim_, SEXP func_) {
  XPtr<Simulator> sim(sim_);
  Function func(func_);
  
  sim->process(func);
}

//[[Rcpp::export]]
void timeout_(SEXP sim_, SEXP delay_) {
  XPtr<Simulator> sim(sim_);
  double delay = as<double>(delay_);
  
  sim->timeout(delay);
}

//[[Rcpp::export]]
void request_(SEXP sim_, SEXP name_, SEXP amount_) {
  XPtr<Simulator> sim(sim_);
  std::string name = as<std::string>(name_);
  int amount = as<int>(amount_);
  
  sim->get_resource(name)->request(amount);
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
int get_res_capacity_(SEXP sim_, SEXP name_){
  XPtr<Simulator> sim(sim_);
  std::string name = as<std::string>(name_);
  
  return sim->get_resource(name)->get_capacity();
}

//[[Rcpp::export]]
int get_res_queue_size_(SEXP sim_, SEXP name_){
  XPtr<Simulator> sim(sim_);
  std::string name = as<std::string>(name_);
  
  return sim->get_resource(name)->get_queue_size();
}
