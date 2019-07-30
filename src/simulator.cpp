// Copyright (C) 2014 Bart Smeets
// Copyright (C) 2015 Iñaki Ucar and Bart Smeets
// Copyright (C) 2015-2019 Iñaki Ucar
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
  XPtr<Simulator>(sim_)->reset();
}

//[[Rcpp::export]]
double now_(SEXP sim_) {
  return XPtr<Simulator>(sim_)->now();
}

//[[Rcpp::export]]
DataFrame peek_(SEXP sim_, int steps) {
  return XPtr<Simulator>(sim_)->peek(steps);
}

//[[Rcpp::export]]
void stepn_(SEXP sim_, unsigned int n) {
  XPtr<Simulator>(sim_)->step(n);
}

//[[Rcpp::export]]
void run_(SEXP sim_, double until) {
  XPtr<Simulator>(sim_)->run(until);
}

//[[Rcpp::export]]
bool add_generator_(SEXP sim_, const std::string& name_prefix, const Environment& trj,
                    const Function& dist, int mon, int priority, int preemptible, bool restart)
{
  XPtr<Simulator> sim(sim_);

  simmer::Generator* gen = new simmer::Generator(
    sim, name_prefix, mon, trj, dist, Order(priority, preemptible, restart));

  bool ret = sim->add_process(gen);

  if (!ret) delete gen;
  return ret;
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

  DataSrc* gen = new DataSrc(
    sim, name_prefix, mon, trj, data, batch, time, attrs,
    priority.empty() ? NONE : boost::make_optional(priority[0]),
    preemptible.empty() ? NONE : boost::make_optional(preemptible[0]),
    restart.empty() ? NONE : boost::make_optional(restart[0]));

  bool ret = sim->add_process(gen);

  if (!ret) delete gen;
  return ret;
}

//[[Rcpp::export]]
bool add_resource_(SEXP sim_, const std::string& name, int capacity, int queue_size,
                   bool mon, bool preemptive, const std::string& preempt_order,
                   bool queue_size_strict, int queue_min_priority)
{
  XPtr<Simulator> sim(sim_);

  Resource* res;
  if (!preemptive) {
    res = new PriorityRes<FIFO>(sim, name, mon, capacity, queue_size,
                                queue_size_strict, queue_min_priority);
  } else {
    if (preempt_order.compare("fifo") == 0)
      res = new PreemptiveRes<FIFO>(sim, name, mon, capacity, queue_size,
                                    queue_size_strict, queue_min_priority);
    else
      res = new PreemptiveRes<LIFO>(sim, name, mon, capacity, queue_size,
                                    queue_size_strict, queue_min_priority);
  }

  bool ret = sim->add_resource(res);

  if (!ret) delete res;
  return ret;
}

//[[Rcpp::export]]
bool add_resource_manager_(SEXP sim_, const std::string& name,
                           const std::string& param, int init,
                           const std::vector<double>& intervals,
                           const std::vector<int>& values, int period)
{
  XPtr<Simulator> sim(sim_);

  Manager<int>* manager;
  Resource* res = sim->get_resource(name);
  std::string manager_name = name + "_" + param;
  if (param.compare("capacity") == 0)
    manager = new Manager<int>(sim, manager_name, intervals, values, period,
                               BIND(&Resource::set_capacity, res, _1), init);
  else
    manager = new Manager<int>(sim, manager_name, intervals, values, period,
                               BIND(&Resource::set_queue_size, res, _1), init);

  bool ret = sim->add_process(manager);

  if (!ret) {
    delete manager;
    stop("resource '%s' was defined, but no schedule was attached", name);
  }
  return ret;
}

//[[Rcpp::export]]
bool add_global_manager_(SEXP sim_, const std::string& key, double init,
                         const std::vector<double>& intervals,
                         const std::vector<double>& values, int period)
{
  XPtr<Simulator> sim(sim_);

  Manager<double>* manager =
    new Manager<double>(sim, key, intervals, values, period,
                        BIND(&Simulator::set_attribute, sim.get(), key, _1), init);

  bool ret = sim->add_process(manager);

  if (!ret) delete manager;
  return ret;
}

//[[Rcpp::export]]
void record_ongoing_(SEXP sim_, bool per_resource) {
  XPtr<Simulator>(sim_)->record_ongoing(per_resource);
}
