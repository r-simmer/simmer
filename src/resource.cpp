#include <simmer.h>

using namespace Rcpp;

//[[Rcpp::export]]
int get_capacity_(SEXP sim_, const std::string& name) {
  XPtr<Simulator> sim(sim_);
  return sim->get_resource(name)->get_capacity();
}

//[[Rcpp::export]]
int get_capacity_selected_(SEXP sim_, int id) {
  XPtr<Simulator> sim(sim_);
  return sim->get_running_arrival()->get_resource_selected(id)->get_capacity();
}

//[[Rcpp::export]]
int get_queue_size_(SEXP sim_, const std::string& name) {
  XPtr<Simulator> sim(sim_);
  return sim->get_resource(name)->get_queue_size();
}

//[[Rcpp::export]]
int get_queue_size_selected_(SEXP sim_, int id) {
  XPtr<Simulator> sim(sim_);
  return sim->get_running_arrival()->get_resource_selected(id)->get_queue_size();
}

//[[Rcpp::export]]
int get_server_count_(SEXP sim_, const std::string& name) {
  XPtr<Simulator> sim(sim_);
  return sim->get_resource(name)->get_server_count();
}

//[[Rcpp::export]]
int get_server_count_selected_(SEXP sim_, int id) {
  XPtr<Simulator> sim(sim_);
  return sim->get_running_arrival()->get_resource_selected(id)->get_server_count();
}

//[[Rcpp::export]]
int get_queue_count_(SEXP sim_, const std::string& name) {
  XPtr<Simulator> sim(sim_);
  return sim->get_resource(name)->get_queue_count();
}

//[[Rcpp::export]]
int get_queue_count_selected_(SEXP sim_, int id) {
  XPtr<Simulator> sim(sim_);
  return sim->get_running_arrival()->get_resource_selected(id)->get_queue_count();
}
