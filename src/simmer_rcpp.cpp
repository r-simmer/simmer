#include "simmer.h"
#include "activity.h"
#include "entity.h"
#include "process.h"
#include "resource.h"
#include "stats.h"
#include "simulator.h"

using namespace Rcpp;

//[[Rcpp::export]]
SEXP Simulator__new(std::string name, bool verbose) {
  return XPtr<Simulator>(new Simulator(name, verbose));
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
List peek_(SEXP sim_, int steps) {
  XPtr<Simulator> sim(sim_);
  std::pair<VEC<double>, VEC<std::string> > ret = sim->peek(steps);

  return List::create(
    Rcpp::Named("time")     = ret.first,
    Rcpp::Named("process")  = ret.second
  );
}

//[[Rcpp::export]]
void step_(SEXP sim_) {
  XPtr<Simulator> sim(sim_);
  sim->step();
}

//[[Rcpp::export]]
void run_(SEXP sim_, double until) {
  XPtr<Simulator> sim(sim_);
  sim->run(until);
}

//[[Rcpp::export]]
bool add_generator_(SEXP sim_, std::string name_prefix, Environment trj,
                    Function dist, int mon, int priority, int preemptible, bool restart)
{
  XPtr<Simulator> sim(sim_);
  return sim->add_generator(name_prefix, trj, dist, mon, priority, preemptible, restart);
}

//[[Rcpp::export]]
bool add_resource_(SEXP sim_, std::string name, int capacity, int queue_size, bool mon,
                   bool preemptive, std::string preempt_order, bool keep_queue)
{
  XPtr<Simulator> sim(sim_);
  return sim->add_resource(name, capacity, queue_size, mon, preemptive, preempt_order, keep_queue);
}

//[[Rcpp::export]]
bool add_resource_manager_(SEXP sim_, std::string name, std::string param,
                           std::vector<double> intervals, std::vector<int> values, int period)
{
  XPtr<Simulator> sim(sim_);
  return sim->add_resource_manager(name, param, intervals, values, period);
}

//[[Rcpp::export]]
List get_mon_arrivals_(SEXP sim_, bool per_resource, bool ongoing) {
  XPtr<Simulator> sim(sim_);
  return sim->get_arr_stats(per_resource, ongoing);
}

//[[Rcpp::export]]
List get_mon_attributes_(SEXP sim_) {
  XPtr<Simulator> sim(sim_);
  return sim->get_attr_stats();
}

//[[Rcpp::export]]
List get_mon_resource_(SEXP sim_) {
  XPtr<Simulator> sim(sim_);
  return sim->get_res_stats();
}

//[[Rcpp::export]]
List get_mon_resource_counts_(SEXP sim_) {
  XPtr<Simulator> sim(sim_);
  return sim->get_res_stats_counts();
}

//[[Rcpp::export]]
List get_mon_resource_limits_(SEXP sim_) {
  XPtr<Simulator> sim(sim_);
  return sim->get_res_stats_limits();
}

//[[Rcpp::export]]
int get_n_generated_(SEXP sim_, std::string name) {
  XPtr<Simulator> sim(sim_);
  return sim->get_generator(name)->get_n_generated();
}

//[[Rcpp::export]]
int get_capacity_(SEXP sim_, std::string name) {
  XPtr<Simulator> sim(sim_);
  return sim->get_resource(name)->get_capacity();
}

//[[Rcpp::export]]
int get_queue_size_(SEXP sim_, std::string name) {
  XPtr<Simulator> sim(sim_);
  return sim->get_resource(name)->get_queue_size();
}

//[[Rcpp::export]]
int get_server_count_(SEXP sim_, std::string name) {
  XPtr<Simulator> sim(sim_);
  return sim->get_resource(name)->get_server_count();
}

//[[Rcpp::export]]
int get_queue_count_(SEXP sim_, std::string name) {
  XPtr<Simulator> sim(sim_);
  return sim->get_resource(name)->get_queue_count();
}

//[[Rcpp::export]]
SEXP Seize__new(bool verbose, std::string resource, int amount,
                std::vector<bool> cont, std::vector<Environment> trj, unsigned short mask)
{
  return XPtr<Seize<int> >(new Seize<int>(verbose, resource, amount, 0, cont, trj, mask));
}

//[[Rcpp::export]]
SEXP Seize__new_func(bool verbose, std::string resource, Function amount, bool provide_attrs,
                     std::vector<bool> cont, std::vector<Environment> trj, unsigned short mask)
{
  return XPtr<Seize<Function> >(
      new Seize<Function>(verbose, resource, amount, provide_attrs, cont, trj, mask));
}

//[[Rcpp::export]]
SEXP SeizeSelected__new(bool verbose, int id, int amount,
                        std::vector<bool> cont, std::vector<Environment> trj, unsigned short mask)
{
  return XPtr<SeizeSelected<int> >(
      new SeizeSelected<int>(verbose, id, amount, 0, cont, trj, mask));
}

//[[Rcpp::export]]
SEXP SeizeSelected__new_func(bool verbose, int id, Function amount, bool provide_attrs,
                             std::vector<bool> cont, std::vector<Environment> trj, unsigned short mask)
{
  return XPtr<SeizeSelected<Function> >(
      new SeizeSelected<Function>(verbose, id, amount, provide_attrs, cont, trj, mask));
}

//[[Rcpp::export]]
SEXP Release__new(bool verbose, std::string resource, int amount) {
  return XPtr<Release<int> >(new Release<int>(verbose, resource, amount, 0));
}

//[[Rcpp::export]]
SEXP Release__new_func(bool verbose, std::string resource, Function amount, bool provide_attrs) {
  return XPtr<Release<Function> >(new Release<Function>(verbose, resource, amount, provide_attrs));
}

//[[Rcpp::export]]
SEXP ReleaseSelected__new(bool verbose, int id, int amount) {
  return XPtr<ReleaseSelected<int> >(new ReleaseSelected<int>(verbose, id, amount, 0));
}

//[[Rcpp::export]]
SEXP ReleaseSelected__new_func(bool verbose, int id, Function amount, bool provide_attrs) {
  return XPtr<ReleaseSelected<Function> >(
      new ReleaseSelected<Function>(verbose, id, amount, provide_attrs));
}

//[[Rcpp::export]]
SEXP SetCapacity__new(bool verbose, std::string resource, double value) {
  return XPtr<SetCapacity<double> >(new SetCapacity<double>(verbose, resource, value, 0));
}

//[[Rcpp::export]]
SEXP SetCapacity__new_func(bool verbose, std::string resource, Function value, bool provide_attrs) {
  return XPtr<SetCapacity<Function> >(
      new SetCapacity<Function>(verbose, resource, value, provide_attrs));
}

//[[Rcpp::export]]
SEXP SetCapacitySelected__new(bool verbose, int id, double value) {
  return XPtr<SetCapacitySelected<double> >(new SetCapacitySelected<double>(verbose, id, value, 0));
}

//[[Rcpp::export]]
SEXP SetCapacitySelected__new_func(bool verbose, int id, Function value, bool provide_attrs) {
  return XPtr<SetCapacitySelected<Function> >(
      new SetCapacitySelected<Function>(verbose, id, value, provide_attrs));
}

//[[Rcpp::export]]
SEXP SetQueue__new(bool verbose, std::string resource, double value) {
  return XPtr<SetQueue<double> >(new SetQueue<double>(verbose, resource, value, 0));
}

//[[Rcpp::export]]
SEXP SetQueue__new_func(bool verbose, std::string resource, Function value, bool provide_attrs) {
  return XPtr<SetQueue<Function> >(
      new SetQueue<Function>(verbose, resource, value, provide_attrs));
}

//[[Rcpp::export]]
SEXP SetQueueSelected__new(bool verbose, int id, double value) {
  return XPtr<SetQueueSelected<double> >(new SetQueueSelected<double>(verbose, id, value, 0));
}

//[[Rcpp::export]]
SEXP SetQueueSelected__new_func(bool verbose, int id, Function value, bool provide_attrs) {
  return XPtr<SetQueueSelected<Function> >(
      new SetQueueSelected<Function>(verbose, id, value, provide_attrs));
}

//[[Rcpp::export]]
SEXP Select__new(bool verbose, std::vector<std::string> resources, std::string policy, int id) {
  return XPtr<Select<VEC<std::string> > >(
      new Select<VEC<std::string> >(verbose, resources, 0, policy, id));
}

//[[Rcpp::export]]
SEXP Select__new_func(bool verbose, Function resources, bool provide_attrs, int id) {
  return XPtr<Select<Function> >(
      new Select<Function>(verbose, resources, provide_attrs, "custom", id));
}

//[[Rcpp::export]]
SEXP SetAttribute__new(bool verbose, std::string key, double value) {
  return XPtr<SetAttribute<double> >(new SetAttribute<double>(verbose, key, value, 0));
}

//[[Rcpp::export]]
SEXP SetAttribute__new_func(bool verbose, std::string key, Function value, bool provide_attrs) {
  return XPtr<SetAttribute<Function> >(
      new SetAttribute<Function>(verbose, key, value, provide_attrs));
}

//[[Rcpp::export]]
SEXP Activate__new(bool verbose, std::string generator) {
  return XPtr<Activate<std::string> >(new Activate<std::string>(verbose, generator, 0));
}

//[[Rcpp::export]]
SEXP Activate__new_func(bool verbose, Function generator, bool provide_attrs) {
  return XPtr<Activate<Function> >(
      new Activate<Function>(verbose, generator, provide_attrs));
}

//[[Rcpp::export]]
SEXP Deactivate__new(bool verbose, std::string generator) {
  return XPtr<Deactivate<std::string> >(new Deactivate<std::string>(verbose, generator, 0));
}

//[[Rcpp::export]]
SEXP Deactivate__new_func(bool verbose, Function generator, bool provide_attrs) {
  return XPtr<Deactivate<Function> >(
      new Deactivate<Function>(verbose, generator, provide_attrs));
}

//[[Rcpp::export]]
SEXP SetTraj__new(bool verbose, std::string generator, Environment trj) {
  return XPtr<SetTraj<std::string> >(new SetTraj<std::string>(verbose, generator, 0, trj));
}

//[[Rcpp::export]]
SEXP SetTraj__new_func(bool verbose, Function generator, bool provide_attrs, Environment trj) {
  return XPtr<SetTraj<Function> >(
      new SetTraj<Function>(verbose, generator, provide_attrs, trj));
}

//[[Rcpp::export]]
SEXP SetDist__new(bool verbose, std::string generator, Function dist) {
  return XPtr<SetDist<std::string> >(new SetDist<std::string>(verbose, generator, 0, dist));
}

//[[Rcpp::export]]
SEXP SetDist__new_func(bool verbose, Function generator, bool provide_attrs, Function dist) {
  return XPtr<SetDist<Function> >(
      new SetDist<Function>(verbose, generator, provide_attrs, dist));
}

//[[Rcpp::export]]
SEXP SetPrior__new(bool verbose, std::vector<int> values) {
  return XPtr<SetPrior<VEC<int> > >(new SetPrior<VEC<int> >(verbose, values, 0));
}

//[[Rcpp::export]]
SEXP SetPrior__new_func(bool verbose, Function values, bool provide_attrs) {
  return XPtr<SetPrior<Function> >(new SetPrior<Function>(verbose, values, provide_attrs));
}

//[[Rcpp::export]]
SEXP Timeout__new(bool verbose, double delay) {
  return XPtr<Timeout<double> >(new Timeout<double>(verbose, delay, 0));
}

//[[Rcpp::export]]
SEXP Timeout__new_func(bool verbose, Function task, bool provide_attrs) {
  return XPtr<Timeout<Function> >(new Timeout<Function>(verbose, task, provide_attrs));
}

//[[Rcpp::export]]
SEXP Branch__new(bool verbose, Function option, bool provide_attrs,
                 std::vector<bool> cont, std::vector<Environment> trj)
{
  return XPtr<Branch>(new Branch(verbose, option, provide_attrs, cont, trj));
}

//[[Rcpp::export]]
SEXP Rollback__new(bool verbose, int amount, int times) {
  return XPtr<Rollback>(new Rollback(verbose, amount, times));
}

//[[Rcpp::export]]
SEXP Rollback__new_func(bool verbose, int amount, Function check, bool provide_attrs) {
  return XPtr<Rollback>(new Rollback(verbose, amount, 0, check, provide_attrs));
}

//[[Rcpp::export]]
SEXP Leave__new(bool verbose, double prob) {
  return XPtr<Leave<double> >(new Leave<double>(verbose, prob, 0));
}

//[[Rcpp::export]]
SEXP Leave__new_func(bool verbose, Function prob, bool provide_attrs) {
  return XPtr<Leave<Function> >(new Leave<Function>(verbose, prob, provide_attrs));
}

//[[Rcpp::export]]
SEXP Clone__new(bool verbose, int n, std::vector<Environment> trj) {
  return XPtr<Clone<int> >(new Clone<int>(verbose, n, 0, trj));
}

//[[Rcpp::export]]
SEXP Clone__new_func(bool verbose, Function n, bool provide_attrs, std::vector<Environment> trj) {
  return XPtr<Clone<Function> >(new Clone<Function>(verbose, n, provide_attrs, trj));
}

//[[Rcpp::export]]
SEXP Synchronize__new(bool verbose, bool wait, bool terminate) {
  return XPtr<Synchronize>(new Synchronize(verbose, wait, terminate));
}

//[[Rcpp::export]]
SEXP Batch__new(bool verbose, int n, double timeout, bool permanent, std::string name) {
  return XPtr<Batch>(new Batch(verbose, n, timeout, permanent, name));
}

//[[Rcpp::export]]
SEXP Batch__new_func(bool verbose, int n, double timeout, bool permanent,
                     std::string name, Function rule, bool provide_attrs)
{
  return XPtr<Batch>(new Batch(verbose, n, timeout, permanent, name, rule, provide_attrs));
}

//[[Rcpp::export]]
SEXP Separate__new(bool verbose) {
  return XPtr<Separate>(new Separate(verbose));
}

//[[Rcpp::export]]
SEXP RenegeIn__new(bool verbose, double t, std::vector<Environment> trj) {
  return XPtr<RenegeIn<double> >(new RenegeIn<double>(verbose, t, 0, trj));
}

//[[Rcpp::export]]
SEXP RenegeIn__new_func(bool verbose, Function t, bool provide_attrs, std::vector<Environment> trj) {
  return XPtr<RenegeIn<Function> >(new RenegeIn<Function>(verbose, t, provide_attrs, trj));
}

//[[Rcpp::export]]
SEXP RenegeIf__new(bool verbose, std::string signal, std::vector<Environment> trj) {
  return XPtr<RenegeIf<std::string> >(new RenegeIf<std::string>(verbose, signal, 0, trj));
}

//[[Rcpp::export]]
SEXP RenegeIf__new_func(bool verbose, Function signal, bool provide_attrs, std::vector<Environment> trj) {
  return XPtr<RenegeIf<Function> >(new RenegeIf<Function>(verbose, signal, provide_attrs, trj));
}

//[[Rcpp::export]]
SEXP RenegeAbort__new(bool verbose) {
  return XPtr<RenegeAbort>(new RenegeAbort(verbose));
}

//[[Rcpp::export]]
SEXP Send__new(bool verbose, std::vector<std::string> signals, double delay) {
  VEC<bool> p = VEC<bool>(2, 0);
  return XPtr<Send<VEC<std::string>, double> >(
      new Send<VEC<std::string>, double>(verbose, signals, delay, p));
}

//[[Rcpp::export]]
SEXP Send__new_func1(bool verbose, Function signals,
                     double delay, bool provide_attrs)
{
  VEC<bool> p = VEC<bool>(2, 0);
  p[0] = provide_attrs;
  return XPtr<Send<Function, double> >(new Send<Function, double>(verbose, signals, delay, p));
}

//[[Rcpp::export]]
SEXP Send__new_func2(bool verbose, std::vector<std::string> signals,
                     Function delay, bool provide_attrs)
{
  VEC<bool> p = VEC<bool>(2, 0);
  p[1] = provide_attrs;
  return XPtr<Send<VEC<std::string>, Function> >(
      new Send<VEC<std::string>, Function>(verbose, signals, delay, p));
}

//[[Rcpp::export]]
SEXP Send__new_func4(bool verbose, Function signals,
                     Function delay, std::vector<bool> provide_attrs)
{
  VEC<bool> p = provide_attrs;
  return XPtr<Send<Function, Function> >(new Send<Function, Function>(verbose, signals, delay, p));
}

//[[Rcpp::export]]
SEXP Trap__new(bool verbose, std::vector<std::string> signals, std::vector<Environment> trj) {
  return XPtr<Trap<VEC<std::string> > >(new Trap<VEC<std::string> >(verbose, signals, 0, trj));
}

//[[Rcpp::export]]
SEXP Trap__new_func(bool verbose, Function signals, bool provide_attrs, std::vector<Environment> trj) {
  return XPtr<Trap<Function> >(new Trap<Function>(verbose, signals, provide_attrs, trj));
}

//[[Rcpp::export]]
SEXP UnTrap__new(bool verbose, std::vector<std::string> signals) {
  return XPtr<UnTrap<VEC<std::string> > >(new UnTrap<VEC<std::string> >(verbose, signals, 0));
}

//[[Rcpp::export]]
SEXP UnTrap__new_func(bool verbose, Function signals, bool provide_attrs) {
  return XPtr<UnTrap<Function> >(new UnTrap<Function>(verbose, signals, provide_attrs));
}

//[[Rcpp::export]]
SEXP Wait__new(bool verbose) {
  return XPtr<Wait>(new Wait(verbose));
}

//[[Rcpp::export]]
SEXP Log__new(bool verbose, std::string message) {
  return XPtr<Log<std::string> >(new Log<std::string>(verbose, message, 0));
}

//[[Rcpp::export]]
SEXP Log__new_func(bool verbose, Function message, bool provide_attrs) {
  return XPtr<Log<Function> >(
      new Log<Function>(verbose, message, provide_attrs));
}

//[[Rcpp::export]]
int activity_get_n_(SEXP activity_) {
  XPtr<Activity> activity(activity_);
  return activity->n;
}

//[[Rcpp::export]]
void activity_print_(SEXP activity_, int indent) {
  XPtr<Activity> activity(activity_);
  return activity->print(indent);
}

//[[Rcpp::export]]
SEXP activity_get_next_(SEXP activity_) {
  XPtr<Activity> activity(activity_);
  Activity* the_next = activity->get_next();
  if (the_next)
    return XPtr<Activity>(the_next, false);
  return R_NilValue;
}

//[[Rcpp::export]]
SEXP activity_get_prev_(SEXP activity_) {
  XPtr<Activity> activity(activity_);
  Activity* the_prev = activity->get_prev();
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
  XPtr<Activity> activity(activity_);
  return XPtr<Activity>(activity->clone());
}
