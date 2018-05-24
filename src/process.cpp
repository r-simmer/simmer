#include <simmer.h>

using namespace Rcpp;
using namespace simmer;

//[[Rcpp::export]]
int get_n_generated_(SEXP sim_, const std::string& name) {
  XPtr<Simulator> sim(sim_);
  return sim->get_source(name)->get_n_generated();
}

//[[Rcpp::export]]
std::string get_name_(SEXP sim_) {
  XPtr<Simulator> sim(sim_);
  return sim->get_running_arrival()->name;
}

//[[Rcpp::export]]
NumericVector get_attribute_(SEXP sim_, const std::vector<std::string>& keys, bool global) {
  XPtr<Simulator> sim(sim_);
  NumericVector attrs;

  foreach_ (const std::string& key, keys)
    attrs.push_back(sim->get_running_arrival()->get_attribute(key, global));

  return attrs;
}

//[[Rcpp::export]]
IntegerVector get_prioritization_(SEXP sim_) {
  XPtr<Simulator> sim(sim_);
  Arrival* a = sim->get_running_arrival();
  return IntegerVector::create(
    a->order.get_priority(), a->order.get_preemptible(), (int)a->order.get_restart()
  );
}
