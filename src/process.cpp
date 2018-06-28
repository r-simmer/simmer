// Copyright (C) 2014 Bart Smeets
// Copyright (C) 2015 Bart Smeets and Iñaki Ucar
// Copyright (C) 2015-2018 Iñaki Ucar
//
// This file is part of simmer.
//
// simmer is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// simmer is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with simmer. If not, see <http://www.gnu.org/licenses/>.

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
