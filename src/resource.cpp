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

template <typename T>
VEC<T> get_param(SEXP sim_, const VEC<std::string>& names, const Fn<T(Resource*)>& param) {
  XPtr<Simulator> sim(sim_);
  VEC<T> out;
  foreach_ (const std::string& name, names)
    out.push_back(param(sim->get_resource(name)));
  return out;
}

template <typename T>
VEC<T> get_param(SEXP sim_, int id, const Fn<T(Resource*)>& param) {
  XPtr<Simulator> sim(sim_);
  VEC<T> out;
  if (Resource* r = sim->get_running_arrival()->get_resource_selected(id))
    out.push_back(param(r));
  return out;
}

//[[Rcpp::export]]
std::vector<int> get_capacity_(SEXP sim_, const std::vector<std::string>& names) {
  return get_param<int>(sim_, names, boost::mem_fn(&Resource::get_capacity));
}

//[[Rcpp::export]]
std::vector<int> get_capacity_selected_(SEXP sim_, int id) {
  return get_param<int>(sim_, id, boost::mem_fn(&Resource::get_capacity));
}

//[[Rcpp::export]]
std::vector<int> get_queue_size_(SEXP sim_, const std::vector<std::string>& names) {
  return get_param<int>(sim_, names, boost::mem_fn(&Resource::get_queue_size));
}

//[[Rcpp::export]]
std::vector<int> get_queue_size_selected_(SEXP sim_, int id) {
  return get_param<int>(sim_, id, boost::mem_fn(&Resource::get_queue_size));
}

//[[Rcpp::export]]
std::vector<int> get_server_count_(SEXP sim_, const std::vector<std::string>& names) {
  return get_param<int>(sim_, names, boost::mem_fn(&Resource::get_server_count));
}

//[[Rcpp::export]]
std::vector<int> get_server_count_selected_(SEXP sim_, int id) {
  return get_param<int>(sim_, id, boost::mem_fn(&Resource::get_server_count));
}

//[[Rcpp::export]]
std::vector<int> get_queue_count_(SEXP sim_, const std::vector<std::string>& names) {
  return get_param<int>(sim_, names, boost::mem_fn(&Resource::get_queue_count));
}

//[[Rcpp::export]]
std::vector<int> get_queue_count_selected_(SEXP sim_, int id) {
  return get_param<int>(sim_, id, boost::mem_fn(&Resource::get_queue_count));
}

//[[Rcpp::export]]
std::vector<int> get_seized_(SEXP sim_, const std::vector<std::string>& names) {
  return get_param<int>(sim_, names, BIND(
      &Resource::get_seized, _1, XPtr<Simulator>(sim_)->get_running_arrival()));
}

//[[Rcpp::export]]
std::vector<int> get_seized_selected_(SEXP sim_, int id) {
  return get_param<int>(sim_, id, BIND(
      &Resource::get_seized, _1, XPtr<Simulator>(sim_)->get_running_arrival()));
}

std::string get_name(Resource* res) { return res->name; }

//[[Rcpp::export]]
std::vector<std::string> get_selected_(SEXP sim_, int id) {
  return get_param(sim_, id, Fn<std::string(Resource*)>(get_name));
}
