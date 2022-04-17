// Copyright (C) 2014 Bart Smeets
// Copyright (C) 2015 Iñaki Ucar and Bart Smeets
// Copyright (C) 2015-2022 Iñaki Ucar
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

template <int RTYPE, typename T>
Vector<RTYPE> get_param(SEXP sim_, const VEC<std::string>& names,
                        const Fn<T(Resource*)>& param)
{
  XPtr<Simulator> sim(sim_);
  Vector<RTYPE> out(names.size());
  for (int i = 0; i < out.size(); i++)
    out[i] = param(sim->get_resource(names[i]));
  return out;
}

template <int RTYPE, typename T>
Vector<RTYPE> get_param(SEXP sim_, int id, const Fn<T(Resource*)>& param) {
  XPtr<Simulator> sim(sim_);
  Vector<RTYPE> out;
  if (Resource* r = sim->get_running_arrival()->get_resource_selected(id))
    out.push_back(param(r));
  return out;
}

//[[Rcpp::export]]
SEXP get_capacity_(SEXP sim_, const std::vector<std::string>& names) {
  return get_param<INTSXP,int>(sim_, names, boost::mem_fn(&Resource::get_capacity));
}

//[[Rcpp::export]]
SEXP get_capacity_selected_(SEXP sim_, int id) {
  return get_param<INTSXP,int>(sim_, id, boost::mem_fn(&Resource::get_capacity));
}

//[[Rcpp::export]]
SEXP get_queue_size_(SEXP sim_, const std::vector<std::string>& names) {
  return get_param<INTSXP,int>(sim_, names, boost::mem_fn(&Resource::get_queue_size));
}

//[[Rcpp::export]]
SEXP get_queue_size_selected_(SEXP sim_, int id) {
  return get_param<INTSXP,int>(sim_, id, boost::mem_fn(&Resource::get_queue_size));
}

//[[Rcpp::export]]
SEXP get_server_count_(SEXP sim_, const std::vector<std::string>& names) {
  return get_param<INTSXP,int>(sim_, names, boost::mem_fn(&Resource::get_server_count));
}

//[[Rcpp::export]]
SEXP get_server_count_selected_(SEXP sim_, int id) {
  return get_param<INTSXP,int>(sim_, id, boost::mem_fn(&Resource::get_server_count));
}

//[[Rcpp::export]]
SEXP get_queue_count_(SEXP sim_, const std::vector<std::string>& names) {
  return get_param<INTSXP,int>(sim_, names, boost::mem_fn(&Resource::get_queue_count));
}

//[[Rcpp::export]]
SEXP get_queue_count_selected_(SEXP sim_, int id) {
  return get_param<INTSXP,int>(sim_, id, boost::mem_fn(&Resource::get_queue_count));
}

//[[Rcpp::export]]
SEXP get_seized_(SEXP sim_, const std::vector<std::string>& names) {
  Arrival* arrival = XPtr<Simulator>(sim_)->get_running_arrival();
  return get_param<INTSXP,int>(sim_, names, BIND(&Resource::get_seized, _1, arrival));
}

//[[Rcpp::export]]
SEXP get_seized_selected_(SEXP sim_, int id) {

  Arrival* arrival = XPtr<Simulator>(sim_)->get_running_arrival();
  return get_param<INTSXP,int>(sim_, id, BIND(&Resource::get_seized, _1, arrival));
}

std::string get_name(Resource* res) { return res->name; }

//[[Rcpp::export]]
SEXP get_selected_(SEXP sim_, int id) {
  return get_param<STRSXP>(sim_, id, Fn<std::string(Resource*)>(get_name));
}

//[[Rcpp::export]]
SEXP get_activity_time_(SEXP sim_, const std::vector<std::string>& names) {
  Arrival* arrival = XPtr<Simulator>(sim_)->get_running_arrival();
  Vector<REALSXP> out(names.size());
  if (!names.empty()) for (int i = 0; i < out.size(); i++)
    out[i] = arrival->get_activity_time(names[i]);
  else out.push_back(arrival->get_activity_time());
  return out;
}

//[[Rcpp::export]]
SEXP get_activity_time_selected_(SEXP sim_, int id) {
  Arrival* arrival = XPtr<Simulator>(sim_)->get_running_arrival();
  Vector<REALSXP> out;
  if (Resource* r = arrival->get_resource_selected(id))
    out.push_back(arrival->get_activity_time(r->name));
  return out;
}
