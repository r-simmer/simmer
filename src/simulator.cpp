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
SEXP Simulator__new(const std::string& name, bool verbose, SEXP mon, int log_level) {
  return XPtr<Simulator>(new Simulator(name, verbose, XPtr<Monitor>(mon), log_level));
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
DataFrame peek_(SEXP sim_, int steps) {
  XPtr<Simulator> sim(sim_);
  return sim->peek(steps);
}

//[[Rcpp::export]]
void stepn_(SEXP sim_, unsigned int n) {
  XPtr<Simulator> sim(sim_);
  sim->step(n);
}

//[[Rcpp::export]]
void run_(SEXP sim_, double until) {
  XPtr<Simulator> sim(sim_);
  sim->run(until);
}

//[[Rcpp::export]]
bool add_generator_(SEXP sim_, const std::string& name_prefix, const Environment& trj,
                    const Function& dist, int mon, int priority, int preemptible, bool restart)
{
  XPtr<Simulator> sim(sim_);
  return sim->add_generator(name_prefix, trj, dist, mon, priority, preemptible, restart);
}

//[[Rcpp::export]]
bool add_dataframe_(SEXP sim_, const std::string& name_prefix, const Environment& trj,
                    const DataFrame& data, int mon, int batch, const std::string& time,
                    const std::vector<std::string>& attrs,
                    const std::vector<std::string>& priority,
                    const std::vector<std::string>& preemptible,
                    const std::vector<std::string>& restart)
{
  XPtr<Simulator> sim(sim_);
  return sim->add_dataframe(name_prefix, trj, data, mon, batch, time, attrs,
                            priority.empty() ? NONE : boost::make_optional(priority[0]),
                            preemptible.empty() ? NONE : boost::make_optional(preemptible[0]),
                            restart.empty() ? NONE : boost::make_optional(restart[0]));
}

//[[Rcpp::export]]
bool add_resource_(SEXP sim_, const std::string& name, int capacity, int queue_size, bool mon,
                   bool preemptive, const std::string& preempt_order, bool queue_size_strict)
{
  XPtr<Simulator> sim(sim_);
  return sim->add_resource(name, capacity, queue_size, mon, preemptive, preempt_order, queue_size_strict);
}

//[[Rcpp::export]]
bool add_resource_manager_(SEXP sim_, const std::string& name, const std::string& param,
                           const std::vector<double>& intervals, const std::vector<int>& values, int period)
{
  XPtr<Simulator> sim(sim_);
  return sim->add_resource_manager(name, param, intervals, values, period);
}

//[[Rcpp::export]]
void record_ongoing_(SEXP sim_, bool per_resource) {
  XPtr<Simulator> sim(sim_);
  sim->record_ongoing(per_resource);
}
