#include "simmer.h"
#include "activity.h"
#include "entity.h"
#include "process.h"
#include "resource.h"
#include "monitor.h"
#include "simulator.h"

using namespace Rcpp;

//[[Rcpp::export]]
SEXP Simulator__new(const std::string& name, bool verbose) {
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
DataFrame peek_(SEXP sim_, int steps) {
  XPtr<Simulator> sim(sim_);
  return sim->peek(steps);
}

//[[Rcpp::export]]
void stepn_(SEXP sim_, unsigned int n) {
  XPtr<Simulator> sim(sim_);
  while (n--) sim->step();
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
bool add_data_(SEXP sim_, const std::string& name_prefix, const Environment& trj,
               const DataFrame& data, int mon, int batch, const std::string& time,
               const std::vector<std::string>& attrs,
               const std::vector<std::string>& priority,
               const std::vector<std::string>& preemptible,
               const std::vector<std::string>& restart)
{
  XPtr<Simulator> sim(sim_);
  return sim->add_data(name_prefix, trj, data, mon, batch, time, attrs,
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
DataFrame get_mon_arrivals_(SEXP sim_, bool per_resource, bool ongoing) {
  XPtr<Simulator> sim(sim_);
  return sim->get_mon_arrivals(per_resource, ongoing);
}

//[[Rcpp::export]]
DataFrame get_mon_attributes_(SEXP sim_) {
  XPtr<Simulator> sim(sim_);
  return sim->get_mon_attributes();
}

//[[Rcpp::export]]
DataFrame get_mon_resources_(SEXP sim_) {
  XPtr<Simulator> sim(sim_);
  return sim->get_mon_resources();
}

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

//[[Rcpp::export]]
SEXP Seize__new(const std::string& resource, int amount, std::vector<bool> cont,
                const std::vector<Environment>& trj, unsigned short mask)
{
  return XPtr<Seize<int> >(new Seize<int>(resource, amount, cont, trj, mask));
}

//[[Rcpp::export]]
SEXP Seize__new_func(const std::string& resource, const Function& amount, std::vector<bool> cont,
                     const std::vector<Environment>& trj, unsigned short mask)
{
  return XPtr<Seize<Function> >(new Seize<Function>(resource, amount, cont, trj, mask));
}

//[[Rcpp::export]]
SEXP SeizeSelected__new(int id, int amount, std::vector<bool> cont,
                        const std::vector<Environment>& trj, unsigned short mask)
{
  return XPtr<Seize<int> >(new Seize<int>(id, amount, cont, trj, mask));
}

//[[Rcpp::export]]
SEXP SeizeSelected__new_func(int id, const Function& amount, std::vector<bool> cont,
                             const std::vector<Environment>& trj, unsigned short mask)
{
  return XPtr<Seize<Function> >(new Seize<Function>(id, amount, cont, trj, mask));
}

//[[Rcpp::export]]
SEXP Release__new(const std::string& resource, int amount) {
  return XPtr<Release<int> >(new Release<int>(resource, amount));
}

//[[Rcpp::export]]
SEXP Release__new_func(const std::string& resource, const Function& amount) {
  return XPtr<Release<Function> >(new Release<Function>(resource, amount));
}

//[[Rcpp::export]]
SEXP ReleaseSelected__new(int id, int amount) {
  return XPtr<Release<int> >(new Release<int>(id, amount));
}

//[[Rcpp::export]]
SEXP ReleaseSelected__new_func(int id, const Function& amount) {
  return XPtr<Release<Function> >(new Release<Function>(id, amount));
}

//[[Rcpp::export]]
SEXP SetCapacity__new(const std::string& resource, double value, char mod) {
  return XPtr<SetCapacity<double> >(new SetCapacity<double>(resource, value, mod));
}

//[[Rcpp::export]]
SEXP SetCapacity__new_func(const std::string& resource, const Function& value, char mod) {
  return XPtr<SetCapacity<Function> >(new SetCapacity<Function>(resource, value, mod));
}

//[[Rcpp::export]]
SEXP SetCapacitySelected__new(int id, double value, char mod) {
  return XPtr<SetCapacity<double> >(new SetCapacity<double>(id, value, mod));
}

//[[Rcpp::export]]
SEXP SetCapacitySelected__new_func(int id, const Function& value, char mod) {
  return XPtr<SetCapacity<Function> >(new SetCapacity<Function>(id, value, mod));
}

//[[Rcpp::export]]
SEXP SetQueue__new(const std::string& resource, double value, char mod) {
  return XPtr<SetQueue<double> >(new SetQueue<double>(resource, value, mod));
}

//[[Rcpp::export]]
SEXP SetQueue__new_func(const std::string& resource, const Function& value, char mod) {
  return XPtr<SetQueue<Function> >(new SetQueue<Function>(resource, value, mod));
}

//[[Rcpp::export]]
SEXP SetQueueSelected__new(int id, double value, char mod) {
  return XPtr<SetQueue<double> >(new SetQueue<double>(id, value, mod));
}

//[[Rcpp::export]]
SEXP SetQueueSelected__new_func(int id, const Function& value, char mod) {
  return XPtr<SetQueue<Function> >(new SetQueue<Function>(id, value, mod));
}

//[[Rcpp::export]]
SEXP Select__new(const std::vector<std::string>& resources, const std::string& policy, int id) {
  return XPtr<Select<VEC<std::string> > >(new Select<VEC<std::string> >(resources, policy, id));
}

//[[Rcpp::export]]
SEXP Select__new_func(const Function& resources, const std::string& policy, int id) {
  return XPtr<Select<Function> >(new Select<Function>(resources, policy, id));
}

//[[Rcpp::export]]
SEXP SetAttribute__new(const std::vector<std::string>& keys,
                       const std::vector<double>& values, bool global, char mod)
{
  return XPtr<SetAttribute<VEC<std::string>, VEC<double> > >(
      new SetAttribute<VEC<std::string>, VEC<double> >(keys, values, global, mod));
}

//[[Rcpp::export]]
SEXP SetAttribute__new_func1(const Function& keys,
                             const std::vector<double>& values, bool global, char mod)
{
  return XPtr<SetAttribute<Function, VEC<double> > >(
      new SetAttribute<Function, VEC<double> >(keys, values, global, mod));
}

//[[Rcpp::export]]
SEXP SetAttribute__new_func2(const std::vector<std::string>& keys,
                             const Function& values, bool global, char mod)
{
  return XPtr<SetAttribute<VEC<std::string>, Function> >(
      new SetAttribute<VEC<std::string>, Function>(keys, values, global, mod));
}

//[[Rcpp::export]]
SEXP SetAttribute__new_func3(const Function& keys,
                             const Function& values, bool global, char mod)
{
  return XPtr<SetAttribute<Function, Function> >(
      new SetAttribute<Function, Function>(keys, values, global, mod));
}

//[[Rcpp::export]]
SEXP Activate__new(const std::string& source) {
  return XPtr<Activate<std::string> >(new Activate<std::string>(source));
}

//[[Rcpp::export]]
SEXP Activate__new_func(const Function& source) {
  return XPtr<Activate<Function> >(new Activate<Function>(source));
}

//[[Rcpp::export]]
SEXP Deactivate__new(const std::string& source) {
  return XPtr<Deactivate<std::string> >(new Deactivate<std::string>(source));
}

//[[Rcpp::export]]
SEXP Deactivate__new_func(const Function& source) {
  return XPtr<Deactivate<Function> >(new Deactivate<Function>(source));
}

//[[Rcpp::export]]
SEXP SetTraj__new(const std::string& source, const Environment& trj) {
  return XPtr<SetTraj<std::string> >(new SetTraj<std::string>(source, trj));
}

//[[Rcpp::export]]
SEXP SetTraj__new_func(const Function& source, const Environment& trj) {
  return XPtr<SetTraj<Function> >(new SetTraj<Function>(source, trj));
}

//[[Rcpp::export]]
SEXP SetSourceFn__new(const std::string& source, const Function& dist) {
  return XPtr<SetSource<std::string, Function> >(
      new SetSource<std::string, Function>(source, dist));
}

//[[Rcpp::export]]
SEXP SetSourceFn__new_func(const Function& source, const Function& dist) {
  return XPtr<SetSource<Function, Function> >(
      new SetSource<Function, Function>(source, dist));
}

//[[Rcpp::export]]
SEXP SetSourceDF__new(const std::string& source, const DataFrame& data) {
  return XPtr<SetSource<std::string, DataFrame> >(
      new SetSource<std::string, DataFrame>(source, data));
}

//[[Rcpp::export]]
SEXP SetSourceDF__new_func(const Function& source, const DataFrame& data) {
  return XPtr<SetSource<Function, DataFrame> >(
      new SetSource<Function, DataFrame>(source, data));
}

//[[Rcpp::export]]
SEXP SetPrior__new(const std::vector<int>& values, char mod) {
  return XPtr<SetPrior<VEC<int> > >(new SetPrior<VEC<int> >(values, mod));
}

//[[Rcpp::export]]
SEXP SetPrior__new_func(const Function& values, char mod) {
  return XPtr<SetPrior<Function> >(new SetPrior<Function>(values, mod));
}

//[[Rcpp::export]]
SEXP Timeout__new(double delay) {
  return XPtr<Timeout<double> >(new Timeout<double>(delay));
}

//[[Rcpp::export]]
SEXP Timeout__new_func(const Function& task) {
  return XPtr<Timeout<Function> >(new Timeout<Function>(task));
}

//[[Rcpp::export]]
SEXP Timeout__new_attr(const std::string& key, bool global) {
  typedef FnWrap<double, Arrival*, std::string> Callback;
  Callback call = Callback(BIND(&Arrival::get_attribute, _1, key, global), key);
  return XPtr<Timeout<Callback> >(new Timeout<Callback>(call));
}

//[[Rcpp::export]]
SEXP Branch__new(const Function& option, std::vector<bool> cont,
                 const std::vector<Environment>& trj)
{
  return XPtr<Branch>(new Branch(option, cont, trj));
}

//[[Rcpp::export]]
SEXP Rollback__new(int amount, int times) {
  return XPtr<Rollback>(new Rollback(amount, times));
}

//[[Rcpp::export]]
SEXP Rollback__new_func(int amount, const Function& check) {
  return XPtr<Rollback>(new Rollback(amount, 0, check));
}

//[[Rcpp::export]]
SEXP Leave__new(double prob) {
  return XPtr<Leave<double> >(new Leave<double>(prob));
}

//[[Rcpp::export]]
SEXP Leave__new_func(const Function& prob) {
  return XPtr<Leave<Function> >(new Leave<Function>(prob));
}

//[[Rcpp::export]]
SEXP Clone__new(int n, const std::vector<Environment>& trj) {
  return XPtr<Clone<int> >(new Clone<int>(n, trj));
}

//[[Rcpp::export]]
SEXP Clone__new_func(const Function& n, const std::vector<Environment>& trj) {
  return XPtr<Clone<Function> >(new Clone<Function>(n, trj));
}

//[[Rcpp::export]]
SEXP Synchronize__new(bool wait, bool terminate) {
  return XPtr<Synchronize>(new Synchronize(wait, terminate));
}

//[[Rcpp::export]]
SEXP Batch__new(int n, double timeout, bool permanent, const std::string& name) {
  return XPtr<Batch<double> >(new Batch<double>(n, timeout, permanent, name));
}

//[[Rcpp::export]]
SEXP Batch__new_func1(int n, const Function& timeout, bool permanent, const std::string& name) {
  return XPtr<Batch<Function> >(new Batch<Function>(n, timeout, permanent, name));
}

//[[Rcpp::export]]
SEXP Batch__new_func2(int n, double timeout, bool permanent,
                      const std::string& name, const Function& rule)
{
  return XPtr<Batch<double> >(new Batch<double>(n, timeout, permanent, name, rule));
}

//[[Rcpp::export]]
SEXP Batch__new_func3(int n, const Function& timeout, bool permanent,
                      const std::string& name,const Function& rule)
{
  return XPtr<Batch<Function> >(new Batch<Function>(n, timeout, permanent, name, rule));
}

//[[Rcpp::export]]
SEXP Separate__new() { return XPtr<Separate>(new Separate()); }

//[[Rcpp::export]]
SEXP RenegeIn__new(double t, const std::vector<Environment>& trj) {
  return XPtr<RenegeIn<double> >(new RenegeIn<double>(t, trj));
}

//[[Rcpp::export]]
SEXP RenegeIn__new_func(const Function& t, const std::vector<Environment>& trj) {
  return XPtr<RenegeIn<Function> >(new RenegeIn<Function>(t, trj));
}

//[[Rcpp::export]]
SEXP RenegeIf__new(const std::string& signal, const std::vector<Environment>& trj) {
  return XPtr<RenegeIf<std::string> >(new RenegeIf<std::string>(signal, trj));
}

//[[Rcpp::export]]
SEXP RenegeIf__new_func(const Function& signal, const std::vector<Environment>& trj) {
  return XPtr<RenegeIf<Function> >(new RenegeIf<Function>(signal, trj));
}

//[[Rcpp::export]]
SEXP RenegeAbort__new() { return XPtr<RenegeAbort>(new RenegeAbort()); }

//[[Rcpp::export]]
SEXP Send__new(const std::vector<std::string>& signals, double delay) {
  return XPtr<Send<VEC<std::string>, double> >(
      new Send<VEC<std::string>, double>(signals, delay));
}

//[[Rcpp::export]]
SEXP Send__new_func1(const Function& signals, double delay) {
  return XPtr<Send<Function, double> >(new Send<Function, double>(signals, delay));
}

//[[Rcpp::export]]
SEXP Send__new_func2(const std::vector<std::string>& signals, const Function& delay) {
  return XPtr<Send<VEC<std::string>, Function> >(
      new Send<VEC<std::string>, Function>(signals, delay));
}

//[[Rcpp::export]]
SEXP Send__new_func3(const Function& signals, const Function& delay) {
  return XPtr<Send<Function, Function> >(new Send<Function, Function>(signals, delay));
}

//[[Rcpp::export]]
SEXP Trap__new(const std::vector<std::string>& signals,
               const std::vector<Environment>& trj, bool interruptible)
{
  return XPtr<Trap<VEC<std::string> > >(
      new Trap<VEC<std::string> >(signals, trj, interruptible));
}

//[[Rcpp::export]]
SEXP Trap__new_func(const Function& signals,
                    const std::vector<Environment>& trj, bool interruptible)
{
  return XPtr<Trap<Function> >(new Trap<Function>(signals, trj, interruptible));
}

//[[Rcpp::export]]
SEXP UnTrap__new(const std::vector<std::string>& signals) {
  return XPtr<UnTrap<VEC<std::string> > >(new UnTrap<VEC<std::string> >(signals));
}

//[[Rcpp::export]]
SEXP UnTrap__new_func(const Function& signals) {
  return XPtr<UnTrap<Function> >(new UnTrap<Function>(signals));
}

//[[Rcpp::export]]
SEXP Wait__new() { return XPtr<Wait>(new Wait()); }

//[[Rcpp::export]]
SEXP Log__new(const std::string& message) {
  return XPtr<Log<std::string> >(new Log<std::string>(message));
}

//[[Rcpp::export]]
SEXP Log__new_func(const Function& message) {
  return XPtr<Log<Function> >(new Log<Function>(message));
}

//[[Rcpp::export]]
int activity_get_count_(SEXP activity_) {
  XPtr<Activity> activity(activity_);
  return activity->count;
}

//[[Rcpp::export]]
void activity_print_(SEXP activity_, int indent, bool verbose) {
  XPtr<Activity> activity(activity_);
  return activity->print(indent, verbose);
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
