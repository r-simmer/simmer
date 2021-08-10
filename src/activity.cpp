// Copyright (C) 2014 Bart Smeets
// Copyright (C) 2015 Iñaki Ucar and Bart Smeets
// Copyright (C) 2015-2021 Iñaki Ucar
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
SEXP Seize__new(const std::string& resource, int amount, std::vector<bool> cont,
                const std::vector<Environment>& trj, unsigned short mask)
{
  return XPtr<Activity>(new Seize<int>(resource, amount, cont, trj, mask));
}

//[[Rcpp::export]]
SEXP Seize__new_func(const std::string& resource, const Function& amount, std::vector<bool> cont,
                     const std::vector<Environment>& trj, unsigned short mask)
{
  return XPtr<Activity>(new Seize<Function>(resource, amount, cont, trj, mask));
}

//[[Rcpp::export]]
SEXP SeizeSelected__new(int id, int amount, std::vector<bool> cont,
                        const std::vector<Environment>& trj, unsigned short mask)
{
  return XPtr<Activity>(new Seize<int>(id, amount, cont, trj, mask));
}

//[[Rcpp::export]]
SEXP SeizeSelected__new_func(int id, const Function& amount, std::vector<bool> cont,
                             const std::vector<Environment>& trj, unsigned short mask)
{
  return XPtr<Activity>(new Seize<Function>(id, amount, cont, trj, mask));
}

//[[Rcpp::export]]
SEXP Release__new(const std::string& resource, int amount) {
  return XPtr<Activity>(new Release<int>(resource, amount));
}

//[[Rcpp::export]]
SEXP Release__new_func(const std::string& resource, const Function& amount) {
  return XPtr<Activity>(new Release<Function>(resource, amount));
}

//[[Rcpp::export]]
SEXP ReleaseAll__new(const std::string& resource) {
  return XPtr<Activity>(new Release<int>(resource));
}

//[[Rcpp::export]]
SEXP ReleaseAll__new_void() {
  return XPtr<Activity>(new Release<int>());
}

//[[Rcpp::export]]
SEXP ReleaseSelected__new(int id, int amount) {
  return XPtr<Activity>(new Release<int>(id, amount));
}

//[[Rcpp::export]]
SEXP ReleaseSelected__new_func(int id, const Function& amount) {
  return XPtr<Activity>(new Release<Function>(id, amount));
}

//[[Rcpp::export]]
SEXP ReleaseSelectedAll__new(int id) {
  return XPtr<Activity>(new Release<int>(id));
}

//[[Rcpp::export]]
SEXP SetCapacity__new(const std::string& resource, double value, char mod) {
  return XPtr<Activity>(new SetCapacity<double>(resource, value, mod));
}

//[[Rcpp::export]]
SEXP SetCapacity__new_func(const std::string& resource, const Function& value, char mod) {
  return XPtr<Activity>(new SetCapacity<Function>(resource, value, mod));
}

//[[Rcpp::export]]
SEXP SetCapacitySelected__new(int id, double value, char mod) {
  return XPtr<Activity>(new SetCapacity<double>(id, value, mod));
}

//[[Rcpp::export]]
SEXP SetCapacitySelected__new_func(int id, const Function& value, char mod) {
  return XPtr<Activity>(new SetCapacity<Function>(id, value, mod));
}

//[[Rcpp::export]]
SEXP SetQueue__new(const std::string& resource, double value, char mod) {
  return XPtr<Activity>(new SetQueue<double>(resource, value, mod));
}

//[[Rcpp::export]]
SEXP SetQueue__new_func(const std::string& resource, const Function& value, char mod) {
  return XPtr<Activity>(new SetQueue<Function>(resource, value, mod));
}

//[[Rcpp::export]]
SEXP SetQueueSelected__new(int id, double value, char mod) {
  return XPtr<Activity>(new SetQueue<double>(id, value, mod));
}

//[[Rcpp::export]]
SEXP SetQueueSelected__new_func(int id, const Function& value, char mod) {
  return XPtr<Activity>(new SetQueue<Function>(id, value, mod));
}

//[[Rcpp::export]]
SEXP Select__new(const std::vector<std::string>& resources, const std::string& policy, int id) {
  return XPtr<Activity>(new Select<VEC<std::string> >(resources, policy, id));
}

//[[Rcpp::export]]
SEXP Select__new_func(const Function& resources, const std::string& policy, int id) {
  return XPtr<Activity>(new Select<Function>(resources, policy, id));
}

//[[Rcpp::export]]
SEXP SetAttribute__new(const std::vector<std::string>& keys, const std::vector<double>& values,
                       bool global, char mod, double init)
{
  return XPtr<Activity>(
      new SetAttribute<VEC<std::string>, VEC<double> >(keys, values, global, mod, init));
}

//[[Rcpp::export]]
SEXP SetAttribute__new_func1(const Function& keys, const std::vector<double>& values,
                             bool global, char mod, double init)
{
  return XPtr<Activity>(
      new SetAttribute<Function, VEC<double> >(keys, values, global, mod, init));
}

//[[Rcpp::export]]
SEXP SetAttribute__new_func2(const std::vector<std::string>& keys, const Function& values,
                             bool global, char mod, double init)
{
  return XPtr<Activity>(
      new SetAttribute<VEC<std::string>, Function>(keys, values, global, mod, init));
}

//[[Rcpp::export]]
SEXP SetAttribute__new_func3(const Function& keys, const Function& values,
                             bool global, char mod, double init)
{
  return XPtr<Activity>(
      new SetAttribute<Function, Function>(keys, values, global, mod, init));
}

//[[Rcpp::export]]
SEXP Activate__new(const std::vector<std::string>& sources) {
  return XPtr<Activity>(new Activate<VEC<std::string> >(sources));
}

//[[Rcpp::export]]
SEXP Activate__new_func(const Function& sources) {
  return XPtr<Activity>(new Activate<Function>(sources));
}

//[[Rcpp::export]]
SEXP Deactivate__new(const std::vector<std::string>& sources) {
  return XPtr<Activity>(new Deactivate<VEC<std::string> >(sources));
}

//[[Rcpp::export]]
SEXP Deactivate__new_func(const Function& sources) {
  return XPtr<Activity>(new Deactivate<Function>(sources));
}

//[[Rcpp::export]]
SEXP SetTraj__new(const std::vector<std::string>& sources, const Environment& trj) {
  return XPtr<Activity>(new SetTraj<VEC<std::string> >(sources, trj));
}

//[[Rcpp::export]]
SEXP SetTraj__new_func(const Function& sources, const Environment& trj) {
  return XPtr<Activity>(new SetTraj<Function>(sources, trj));
}

//[[Rcpp::export]]
SEXP SetSourceFn__new(const std::vector<std::string>& sources, const Function& dist) {
  return XPtr<Activity>(new SetSource<VEC<std::string>, Function>(sources, dist));
}

//[[Rcpp::export]]
SEXP SetSourceFn__new_func(const Function& sources, const Function& dist) {
  return XPtr<Activity>(new SetSource<Function, Function>(sources, dist));
}

//[[Rcpp::export]]
SEXP SetSourceDF__new(const std::vector<std::string>& sources, const DataFrame& data) {
  return XPtr<Activity>(new SetSource<VEC<std::string>, DataFrame>(sources, data));
}

//[[Rcpp::export]]
SEXP SetSourceDF__new_func(const Function& sources, const DataFrame& data) {
  return XPtr<Activity>(new SetSource<Function, DataFrame>(sources, data));
}

//[[Rcpp::export]]
SEXP SetPrior__new(const std::vector<int>& values, char mod) {
  return XPtr<Activity>(new SetPrior<VEC<int> >(values, mod));
}

//[[Rcpp::export]]
SEXP SetPrior__new_func(const Function& values, char mod) {
  return XPtr<Activity>(new SetPrior<Function>(values, mod));
}

//[[Rcpp::export]]
SEXP Timeout__new(double delay) {
  return XPtr<Activity>(new Timeout<double>(delay));
}

//[[Rcpp::export]]
SEXP Timeout__new_func(const Function& task) {
  return XPtr<Activity>(new Timeout<Function>(task));
}

//[[Rcpp::export]]
SEXP Timeout__new_attr(const std::string& key, bool global) {
  typedef FnWrap<double, Arrival*, std::string> Callback;
  Callback call = Callback(BIND(&Arrival::get_attribute, _1, key, global), key);
  return XPtr<Activity>(new Timeout<Callback>(call));
}

//[[Rcpp::export]]
SEXP Branch__new(const Function& option, std::vector<bool> cont,
                 const std::vector<Environment>& trj)
{
  return XPtr<Activity>(new Branch(option, cont, trj));
}

//[[Rcpp::export]]
SEXP Rollback__new(int amount, int times) {
  return XPtr<Activity>(new Rollback(amount, times));
}

//[[Rcpp::export]]
SEXP Rollback__new_func(int amount, const Function& check) {
  return XPtr<Activity>(new Rollback(amount, 0, check));
}

//[[Rcpp::export]]
SEXP Leave__new(double prob, const std::vector<Environment>& trj, bool keep_seized) {
  return XPtr<Activity>(new Leave<double>(prob, trj, keep_seized));
}

//[[Rcpp::export]]
SEXP HandleUnfinished__new(const std::vector<Environment>& trj) {
  return XPtr<Activity>(new HandleUnfinished(trj));
}

//[[Rcpp::export]]
SEXP Leave__new_func(const Function& prob, const std::vector<Environment>& trj, bool keep_seized) {
  return XPtr<Activity>(new Leave<Function>(prob, trj, keep_seized));
}

//[[Rcpp::export]]
SEXP Clone__new(int n, const std::vector<Environment>& trj) {
  return XPtr<Activity>(new Clone<int>(n, trj));
}

//[[Rcpp::export]]
SEXP Clone__new_func(const Function& n, const std::vector<Environment>& trj) {
  return XPtr<Activity>(new Clone<Function>(n, trj));
}

//[[Rcpp::export]]
SEXP Synchronize__new(bool wait, bool terminate) {
  return XPtr<Activity>(new Synchronize(wait, terminate));
}

//[[Rcpp::export]]
SEXP Batch__new(int n, double timeout, bool permanent, const std::string& name) {
  return XPtr<Activity>(new Batch<int, double>(n, timeout, permanent, name));
}

//[[Rcpp::export]]
SEXP Batch__new_func1(const Function& n, double timeout, bool permanent,
                      const std::string& name)
{
  return XPtr<Activity>(new Batch<Function, double>(n, timeout, permanent, name));
}

//[[Rcpp::export]]
SEXP Batch__new_func2(int n, const Function& timeout, bool permanent,
                      const std::string& name)
{
  return XPtr<Activity>(new Batch<int, Function>(n, timeout, permanent, name));
}

//[[Rcpp::export]]
SEXP Batch__new_func3(const Function& n, const Function& timeout, bool permanent,
                      const std::string& name)
{
  return XPtr<Activity>(new Batch<Function, Function>(n, timeout, permanent, name));
}

//[[Rcpp::export]]
SEXP Batch__new_func4(int n, double timeout, bool permanent,
                      const std::string& name, const Function& rule)
{
  return XPtr<Activity>(new Batch<int, double>(n, timeout, permanent, name, rule));
}

//[[Rcpp::export]]
SEXP Batch__new_func5(const Function& n, double timeout, bool permanent,
                      const std::string& name, const Function& rule)
{
  return XPtr<Activity>(new Batch<Function, double>(n, timeout, permanent, name, rule));
}

//[[Rcpp::export]]
SEXP Batch__new_func6(int n, const Function& timeout, bool permanent,
                      const std::string& name, const Function& rule)
{
  return XPtr<Activity>(new Batch<int, Function>(n, timeout, permanent, name, rule));
}

//[[Rcpp::export]]
SEXP Batch__new_func7(const Function& n, const Function& timeout, bool permanent,
                      const std::string& name, const Function& rule)
{
  return XPtr<Activity>(new Batch<Function, Function>(n, timeout, permanent, name, rule));
}

//[[Rcpp::export]]
SEXP Separate__new() { return XPtr<Activity>(new Separate()); }

//[[Rcpp::export]]
SEXP RenegeIn__new(double t, const std::vector<Environment>& trj, bool keep_seized) {
  return XPtr<Activity>(new RenegeIn<double>(t, trj, keep_seized));
}

//[[Rcpp::export]]
SEXP RenegeIn__new_func(const Function& t, const std::vector<Environment>& trj, bool keep_seized) {
  return XPtr<Activity>(new RenegeIn<Function>(t, trj, keep_seized));
}

//[[Rcpp::export]]
SEXP RenegeIf__new(const std::string& signal, const std::vector<Environment>& trj, bool keep_seized) {
  return XPtr<Activity>(new RenegeIf<std::string>(signal, trj, keep_seized));
}

//[[Rcpp::export]]
SEXP RenegeIf__new_func(const Function& signal, const std::vector<Environment>& trj, bool keep_seized) {
  return XPtr<Activity>(new RenegeIf<Function>(signal, trj, keep_seized));
}

//[[Rcpp::export]]
SEXP RenegeAbort__new() { return XPtr<Activity>(new RenegeAbort()); }

//[[Rcpp::export]]
SEXP Send__new(const std::vector<std::string>& signals, double delay) {
  return XPtr<Activity>(new Send<VEC<std::string>, double>(signals, delay));
}

//[[Rcpp::export]]
SEXP Send__new_func1(const Function& signals, double delay) {
  return XPtr<Activity>(new Send<Function, double>(signals, delay));
}

//[[Rcpp::export]]
SEXP Send__new_func2(const std::vector<std::string>& signals, const Function& delay) {
  return XPtr<Activity>(new Send<VEC<std::string>, Function>(signals, delay));
}

//[[Rcpp::export]]
SEXP Send__new_func3(const Function& signals, const Function& delay) {
  return XPtr<Activity>(new Send<Function, Function>(signals, delay));
}

//[[Rcpp::export]]
SEXP Trap__new(const std::vector<std::string>& signals,
               const std::vector<Environment>& trj, bool interruptible)
{
  return XPtr<Activity>(new Trap<VEC<std::string> >(signals, trj, interruptible));
}

//[[Rcpp::export]]
SEXP Trap__new_func(const Function& signals,
                    const std::vector<Environment>& trj, bool interruptible)
{
  return XPtr<Activity>(new Trap<Function>(signals, trj, interruptible));
}

//[[Rcpp::export]]
SEXP UnTrap__new(const std::vector<std::string>& signals) {
  return XPtr<Activity>(new UnTrap<VEC<std::string> >(signals));
}

//[[Rcpp::export]]
SEXP UnTrap__new_func(const Function& signals) {
  return XPtr<Activity>(new UnTrap<Function>(signals));
}

//[[Rcpp::export]]
SEXP Wait__new() { return XPtr<Activity>(new Wait()); }

//[[Rcpp::export]]
SEXP Log__new(const std::string& message, int level) {
  return XPtr<Activity>(new Log<std::string>(message, level));
}

//[[Rcpp::export]]
SEXP Log__new_func(const Function& message, int level) {
  return XPtr<Activity>(new Log<Function>(message, level));
}

//[[Rcpp::export]]
SEXP StopIf__new(bool condition) {
  return XPtr<Activity>(new StopIf<bool>(condition));
}

//[[Rcpp::export]]
SEXP StopIf__new_func(const Function& condition) {
  return XPtr<Activity>(new StopIf<Function>(condition));
}

//[[Rcpp::export]]
int activity_get_count_(SEXP activity_) {
  return XPtr<Activity>(activity_)->count;
}

//[[Rcpp::export]]
void activity_print_(SEXP activity_, int indent, bool verbose) {
  return XPtr<Activity>(activity_)->print(indent, verbose);
}

//[[Rcpp::export]]
SEXP activity_get_next_(SEXP activity_) {
  Activity* the_next = XPtr<Activity>(activity_)->get_next();
  if (the_next)
    return XPtr<Activity>(the_next, false);
  return R_NilValue;
}

//[[Rcpp::export]]
SEXP activity_get_prev_(SEXP activity_) {
  Activity* the_prev = XPtr<Activity>(activity_)->get_prev();
  if (the_prev)
    return XPtr<Activity>(the_prev, false);
  return R_NilValue;
}

//[[Rcpp::export]]
void activity_chain_(SEXP first_, SEXP second_) {
  XPtr<Activity> first(first_);
  XPtr<Activity> second(second_);
  first->set_next(second);
  second->set_prev(first);
}

//[[Rcpp::export]]
SEXP activity_clone_(SEXP activity_) {
  return XPtr<Activity>(XPtr<Activity>(activity_)->clone());
}
