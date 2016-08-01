#include "simmer.h"
#include "activity.h"
#include "entity.h"
#include "process.h"
#include "resource.h"
#include "stats.h"
#include "simulator.h"

using namespace Rcpp;

//[[Rcpp::export]]
SEXP Simulator__new(SEXP name_, SEXP verbose_) {
  std::string name = as<std::string>(name_);
  bool verbose = as<bool>(verbose_);
  
  XPtr<Simulator> ptr(new Simulator(name, verbose));
  return ptr;
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
SEXP peek_(SEXP sim_, SEXP steps_) {
  XPtr<Simulator> sim(sim_);
  int steps = as<int>(steps_);
  
  std::pair<VEC<double>, VEC<std::string> > ret = sim->peek(steps);
  
  return Rcpp::List::create(
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
void run_(SEXP sim_, SEXP until_) {
  XPtr<Simulator> sim(sim_);
  double until = as<double>(until_);
  
  sim->run(until);
}

//[[Rcpp::export]]
bool add_generator_(SEXP sim_, SEXP name_prefix_, SEXP first_activity_, SEXP dist_, SEXP mon_,
                    SEXP priority_, SEXP preemptible_, SEXP restart_) {
  XPtr<Simulator> sim(sim_);
  std::string name_prefix = as<std::string>(name_prefix_);
  XPtr<Activity> first_activity(first_activity_);
  Function dist(dist_);
  int mon = as<int>(mon_);
  int priority = as<int>(priority_);
  int preemptible = as<int>(preemptible_);
  bool restart = as<bool>(restart_);
  
  return sim->add_generator(name_prefix, first_activity, dist, mon, priority, preemptible, restart);
}

//[[Rcpp::export]]
bool add_resource_(SEXP sim_, SEXP name_, SEXP capacity_, SEXP queue_size_, SEXP mon_,
                   SEXP preemptive_, SEXP preempt_order_, SEXP keep_queue_) {
  XPtr<Simulator> sim(sim_);
  std::string name = as<std::string>(name_);
  int capacity = as<int>(capacity_);
  int queue_size = as<int>(queue_size_);
  bool mon = as<bool>(mon_);
  bool preemptive = as<bool>(preemptive_);
  std::string preempt_order = as<std::string>(preempt_order_);
  bool keep_queue = as<bool>(keep_queue_);
  
  return sim->add_resource(name, capacity, queue_size, mon, preemptive, preempt_order, keep_queue);
}

//[[Rcpp::export]]
bool add_resource_manager_(SEXP sim_, SEXP name_, SEXP param_, 
                           SEXP intervals_, SEXP values_, SEXP period_) {
  XPtr<Simulator> sim(sim_);
  std::string name = as<std::string>(name_);
  std::string param = as<std::string>(param_);
  VEC<double> intervals = as<VEC<double> >(intervals_);
  VEC<int> values = as<VEC<int> >(values_);
  int period = as<int>(period_);
  
  return sim->add_resource_manager(name, param, intervals, values, period);
}

//[[Rcpp::export]]
SEXP get_mon_arrivals_(SEXP sim_) {
  XPtr<Simulator> sim(sim_);
  
  return sim->get_arr_traj_stats();
}

//[[Rcpp::export]]
SEXP get_mon_arrivals_per_resource_(SEXP sim_) {
  XPtr<Simulator> sim(sim_);

  return sim->get_arr_res_stats();
}

//[[Rcpp::export]]
SEXP get_mon_attributes_(SEXP sim_) {
  XPtr<Simulator> sim(sim_);
  
  return sim->get_attr_stats();
}

//[[Rcpp::export]]
SEXP get_mon_resource_(SEXP sim_) {
  XPtr<Simulator> sim(sim_);
  
  return sim->get_res_stats();
}

//[[Rcpp::export]]
SEXP get_mon_resource_counts_(SEXP sim_) {
  XPtr<Simulator> sim(sim_);
  
  return sim->get_res_stats_counts();
}

//[[Rcpp::export]]
SEXP get_mon_resource_limits_(SEXP sim_) {
  XPtr<Simulator> sim(sim_);

  return sim->get_res_stats_limits();
}

//[[Rcpp::export]]
int get_n_generated_(SEXP sim_, SEXP name_){
  XPtr<Simulator> sim(sim_);
  std::string name = as<std::string>(name_);
  
  return sim->get_generator(name)->get_n_generated();
}

//[[Rcpp::export]]
void set_capacity_(SEXP sim_, SEXP name_, SEXP value_){
  XPtr<Simulator> sim(sim_);
  std::string name = as<std::string>(name_);
  int value = as<int>(value_);
  
  sim->get_resource(name)->set_capacity(value);
}


//[[Rcpp::export]]
int get_capacity_(SEXP sim_, SEXP name_){
  XPtr<Simulator> sim(sim_);
  std::string name = as<std::string>(name_);
  
  return sim->get_resource(name)->get_capacity();
}

//[[Rcpp::export]]
void set_queue_size_(SEXP sim_, SEXP name_, SEXP value_){
  XPtr<Simulator> sim(sim_);
  std::string name = as<std::string>(name_);
  int value = as<int>(value_);
  
  sim->get_resource(name)->set_queue_size(value);
}

//[[Rcpp::export]]
int get_queue_size_(SEXP sim_, SEXP name_){
  XPtr<Simulator> sim(sim_);
  std::string name = as<std::string>(name_);
  
  return sim->get_resource(name)->get_queue_size();
}

//[[Rcpp::export]]
int get_server_count_(SEXP sim_, SEXP name_){
  XPtr<Simulator> sim(sim_);
  std::string name = as<std::string>(name_);
  
  return sim->get_resource(name)->get_server_count();
}

//[[Rcpp::export]]
int get_queue_count_(SEXP sim_, SEXP name_){
  XPtr<Simulator> sim(sim_);
  std::string name = as<std::string>(name_);
  
  return sim->get_resource(name)->get_queue_count();
}

//[[Rcpp::export]]
SEXP Seize__new(SEXP verbose_, SEXP resource_, SEXP amount_, SEXP cont_, SEXP trj_, SEXP mask_) {
  bool verbose = as<bool>(verbose_);
  std::string resource = as<std::string>(resource_);
  int amount = as<int>(amount_);
  VEC<bool> cont = as<VEC<bool> >(cont_);
  VEC<Environment> trj = as<VEC<Environment> >(trj_);
  unsigned short mask = as<unsigned short>(mask_);

  XPtr<Seize<int> > ptr(new Seize<int>(verbose, resource, amount, 0, cont, trj, mask), false);
  return ptr;
}

//[[Rcpp::export]]
SEXP Seize__new_func(SEXP verbose_, SEXP resource_, Function amount, SEXP provide_attrs_, 
                     SEXP cont_, SEXP trj_, SEXP mask_) {
  bool verbose = as<bool>(verbose_);
  std::string resource = as<std::string>(resource_);
  bool provide_attrs = as<bool>(provide_attrs_);
  VEC<bool> cont = as<VEC<bool> >(cont_);
  VEC<Environment> trj = as<VEC<Environment> >(trj_);
  unsigned short mask = as<unsigned short>(mask_);
  
  XPtr<Seize<Function> > 
    ptr(new Seize<Function>(verbose, resource, amount, provide_attrs, cont, trj, mask), false);
  return ptr;
}

//[[Rcpp::export]]
SEXP SeizeSelected__new(SEXP verbose_, SEXP id_, SEXP amount_, SEXP cont_, SEXP trj_, SEXP mask_) {
  bool verbose = as<bool>(verbose_);
  int id = as<int>(id_);
  int amount = as<int>(amount_);
  VEC<bool> cont = as<VEC<bool> >(cont_);
  VEC<Environment> trj = as<VEC<Environment> >(trj_);
  unsigned short mask = as<unsigned short>(mask_);
  
  XPtr<SeizeSelected<int> > 
    ptr(new SeizeSelected<int>(verbose, id, amount, 0, cont, trj, mask), false);
  return ptr;
}

//[[Rcpp::export]]
SEXP SeizeSelected__new_func(SEXP verbose_, SEXP id_, Function amount, SEXP provide_attrs_, 
                             SEXP cont_, SEXP trj_, SEXP mask_) {
  bool verbose = as<bool>(verbose_);
  int id = as<int>(id_);
  bool provide_attrs = as<bool>(provide_attrs_);
  VEC<bool> cont = as<VEC<bool> >(cont_);
  VEC<Environment> trj = as<VEC<Environment> >(trj_);
  unsigned short mask = as<unsigned short>(mask_);
  
  XPtr<SeizeSelected<Function> > 
    ptr(new SeizeSelected<Function>(verbose, id, amount, provide_attrs, cont, trj, mask), false);
  return ptr;
}

//[[Rcpp::export]]
SEXP Release__new(SEXP verbose_, SEXP resource_, SEXP amount_) {
  bool verbose = as<bool>(verbose_);
  std::string resource = as<std::string>(resource_);
  int amount = as<int>(amount_);
  
  XPtr<Release<int> > ptr(new Release<int>(verbose, resource, amount, 0), false);
  return ptr;
}

//[[Rcpp::export]]
SEXP Release__new_func(SEXP verbose_, SEXP resource_, Function amount, SEXP provide_attrs_) {
  bool verbose = as<bool>(verbose_);
  std::string resource = as<std::string>(resource_);
  bool provide_attrs = as<bool>(provide_attrs_);
  
  XPtr<Release<Function> > ptr(new Release<Function>(verbose, resource, amount, provide_attrs), false);
  return ptr;
}

//[[Rcpp::export]]
SEXP ReleaseSelected__new(SEXP verbose_, SEXP id_, SEXP amount_) {
  bool verbose = as<bool>(verbose_);
  int id = as<int>(id_);
  int amount = as<int>(amount_);
  
  XPtr<ReleaseSelected<int> > ptr(new ReleaseSelected<int>(verbose, id, amount, 0), false);
  return ptr;
}

//[[Rcpp::export]]
SEXP ReleaseSelected__new_func(SEXP verbose_, SEXP id_, Function amount, SEXP provide_attrs_) {
  bool verbose = as<bool>(verbose_);
  int id = as<int>(id_);
  bool provide_attrs = as<bool>(provide_attrs_);
  
  XPtr<ReleaseSelected<Function> > 
    ptr(new ReleaseSelected<Function>(verbose, id, amount, provide_attrs), false);
  return ptr;
}

//[[Rcpp::export]]
SEXP Select__new(SEXP verbose_, SEXP resources_, SEXP policy_, SEXP id_) {
  bool verbose = as<bool>(verbose_);
  VEC<std::string> resources = as<VEC<std::string> >(resources_);
  std::string policy = as<std::string>(policy_);
  if (resources.size() == 1) policy = "none";
  int id = as<int>(id_);
  
  XPtr<Select<VEC<std::string> > > 
    ptr(new Select<VEC<std::string> >(verbose, resources, 0, policy, id), false);
  return ptr;
}

//[[Rcpp::export]]
SEXP Select__new_func(SEXP verbose_, Function resources, SEXP provide_attrs_, SEXP id_) {
  bool verbose = as<bool>(verbose_);
  bool provide_attrs = as<bool>(provide_attrs_);
  int id = as<int>(id_);
  
  XPtr<Select<Function> > 
    ptr(new Select<Function>(verbose, resources, provide_attrs, "custom", id), false);
  return ptr;
}

//[[Rcpp::export]]
SEXP SetAttribute__new(SEXP verbose_, SEXP key_, SEXP value_) {
  bool verbose = as<bool>(verbose_);
  std::string key = as<std::string>(key_);
  double value = as<double>(value_);
  
  XPtr<SetAttribute<double> > ptr(new SetAttribute<double>(verbose, key, value, 0), false);
  return ptr;
}

//[[Rcpp::export]]
SEXP SetAttribute__new_func(SEXP verbose_, SEXP key_, Function value, SEXP provide_attrs_) {
  // TODO: look into (dis)advantages of using bool provide_attrs directly instead of SEXP provide_attrs
  bool verbose = as<bool>(verbose_);
  std::string key = as<std::string>(key_);
  bool provide_attrs = as<bool>(provide_attrs_);
  
  XPtr<SetAttribute<Function> > ptr(new SetAttribute<Function>(verbose, key, value, provide_attrs), false);
  return ptr;
}

//[[Rcpp::export]]
SEXP SetPrior__new(SEXP verbose_, SEXP values_) {
  bool verbose = as<bool>(verbose_);
  VEC<int> values = as<VEC<int> >(values_);
  
  XPtr<SetPrior<VEC<int> > > ptr(new SetPrior<VEC<int> >(verbose, values, 0), false);
  return ptr;
}

//[[Rcpp::export]]
SEXP SetPrior__new_func(SEXP verbose_, Function values, SEXP provide_attrs_) {
  bool verbose = as<bool>(verbose_);
  bool provide_attrs = as<bool>(provide_attrs_);
  
  XPtr<SetPrior<Function> > ptr(new SetPrior<Function>(verbose, values, provide_attrs), false);
  return ptr;
}

//[[Rcpp::export]]
SEXP Timeout__new(SEXP verbose_, SEXP delay_) {
  bool verbose = as<bool>(verbose_);
  double delay = as<double>(delay_);
  
  XPtr<Timeout<double> > ptr(new Timeout<double>(verbose, delay, 0), false);
  return ptr;
}

//[[Rcpp::export]]
SEXP Timeout__new_func(SEXP verbose_, Function task, SEXP provide_attrs_) {
  bool verbose = as<bool>(verbose_);
  bool provide_attrs = as<bool>(provide_attrs_);
  
  XPtr<Timeout<Function> > ptr(new Timeout<Function>(verbose, task, provide_attrs), false);
  return ptr;
}

//[[Rcpp::export]]
SEXP Branch__new(SEXP verbose_, Function option, SEXP provide_attrs_, SEXP cont_, SEXP trj_) {
  bool verbose = as<bool>(verbose_);
  bool provide_attrs = as<bool>(provide_attrs_);
  VEC<bool> cont = as<VEC<bool> >(cont_);
  VEC<Environment> trj = as<VEC<Environment> >(trj_);
  
  XPtr<Branch> ptr(new Branch(verbose, option, provide_attrs, cont, trj), false);
  return ptr;
}

//[[Rcpp::export]]
SEXP Rollback__new(SEXP verbose_, SEXP amount_, SEXP times_) {
  bool verbose = as<bool>(verbose_);
  int amount = as<int>(amount_);
  int times = as<int>(times_);
  
  XPtr<Rollback<int> > ptr(new Rollback<int>(verbose, amount, times, 0), false);
  return ptr;
}

//[[Rcpp::export]]
SEXP Rollback__new_func(SEXP verbose_, SEXP amount_, Function check, SEXP provide_attrs_) {
  bool verbose = as<bool>(verbose_);
  int amount = as<int>(amount_);
  bool provide_attrs = as<bool>(provide_attrs_);
  
  XPtr<Rollback<Function> > ptr(new Rollback<Function>(verbose, amount, check, provide_attrs), false);
  return ptr;
}

//[[Rcpp::export]]
SEXP Leave__new(SEXP verbose_, SEXP prob_) {
  bool verbose = as<bool>(verbose_);
  double prob = as<double>(prob_);
  
  XPtr<Leave<double> > ptr(new Leave<double>(verbose, prob, 0), false);
  return ptr;
}

//[[Rcpp::export]]
SEXP Leave__new_func(SEXP verbose_, Function prob, SEXP provide_attrs_) {
  bool verbose = as<bool>(verbose_);
  bool provide_attrs = as<bool>(provide_attrs_);
  
  XPtr<Leave<Function> > ptr(new Leave<Function>(verbose, prob, provide_attrs), false);
  return ptr;
}

//[[Rcpp::export]]
SEXP Clone__new(SEXP verbose_, SEXP n_, SEXP trj_) {
  bool verbose = as<bool>(verbose_);
  int n = as<int>(n_);
  VEC<Environment> trj = as<VEC<Environment> >(trj_);
  
  XPtr<Clone<int> > ptr(new Clone<int>(verbose, n, 0, trj), false);
  return ptr;
}

//[[Rcpp::export]]
SEXP Clone__new_func(SEXP verbose_, Function n, SEXP provide_attrs_, SEXP trj_) {
  bool verbose = as<bool>(verbose_);
  bool provide_attrs = as<bool>(provide_attrs_);
  VEC<Environment> trj = as<VEC<Environment> >(trj_);
  
  XPtr<Clone<Function> > ptr(new Clone<Function>(verbose, n, provide_attrs, trj), false);
  return ptr;
}

//[[Rcpp::export]]
SEXP Synchronize__new(SEXP verbose_, SEXP wait_, SEXP terminate_) {
  bool verbose = as<bool>(verbose_);
  bool wait = as<bool>(wait_);
  bool terminate = as<bool>(terminate_);

  XPtr<Synchronize> ptr(new Synchronize(verbose, wait, terminate), false);
  return ptr;
}

//[[Rcpp::export]]
int activity_get_n_(SEXP activity_) {
  XPtr<Activity> activity(activity_);
  
  return activity->n;
}

//[[Rcpp::export]]
void activity_print_(SEXP activity_, SEXP indent_) {
  XPtr<Activity> activity(activity_);
  int indent = as<int>(indent_);
  
  return activity->print(indent);
}

//[[Rcpp::export]]
SEXP activity_get_next_(SEXP activity_) {
  XPtr<Activity> activity(activity_);
  
  Activity* the_next = activity->get_next();
  if (the_next) {
    XPtr<Activity> ptr(the_next, false);
    return ptr;
  } else return R_NilValue;
}

//[[Rcpp::export]]
SEXP activity_get_prev_(SEXP activity_) {
  XPtr<Activity> activity(activity_);
  
  Activity* the_prev = activity->get_prev();
  if (the_prev) {
    XPtr<Activity> ptr(the_prev, false);
    return ptr;
  } else return R_NilValue;
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
  
  XPtr<Activity> ptr(activity->clone(), false);
  return ptr;
}
