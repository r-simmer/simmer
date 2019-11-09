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

template <int RTYPE, typename T>
Vector<RTYPE> get_param(SEXP sim_, const VEC<std::string>& names,
                        const Fn<T(Source*)>& param)
{
  XPtr<Simulator> sim(sim_);
  Vector<RTYPE> out(names.size());
  for (int i = 0; i < out.size(); i++)
    out[i] = param(sim->get_source(names[i]));
  return out;
}

//[[Rcpp::export]]
SEXP get_n_generated_(SEXP sim_, const std::vector<std::string>& names) {
  return get_param<INTSXP,int>(sim_, names, boost::mem_fn(&Source::get_n_generated));
}

//[[Rcpp::export]]
SEXP get_trajectory_(SEXP sim_, const std::vector<std::string>& names) {
  return get_param<VECSXP,Environment>(sim_, names, boost::mem_fn(&Source::get_trajectory));
}

//[[Rcpp::export]]
std::string get_name_(SEXP sim_) {
  return XPtr<Simulator>(sim_)->get_running_arrival()->name;
}

//[[Rcpp::export]]
SEXP get_attribute_(SEXP sim_, const std::vector<std::string>& keys, bool global) {
  XPtr<Simulator> sim(sim_);
  NumericVector attrs(keys.size());

  if (global) {
    for (int i = 0; i < attrs.size(); i++)
      attrs[i] = sim->get_attribute(keys[i]);
  } else {
    for (int i = 0; i < attrs.size(); i++)
      attrs[i] = sim->get_running_arrival()->get_attribute(keys[i]);
  }

  return attrs;
}

//[[Rcpp::export]]
SEXP get_prioritization_(SEXP sim_) {
  Arrival* a = XPtr<Simulator>(sim_)->get_running_arrival();
  return IntegerVector::create(
    a->order.get_priority(), a->order.get_preemptible(), (int)a->order.get_restart()
  );
}
