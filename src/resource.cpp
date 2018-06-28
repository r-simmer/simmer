// Copyright (C) 2014 Bart Smeets
// Copyright (C) 2015 Iñaki Ucar and Bart Smeets
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
