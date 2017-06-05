#ifndef ACTIVITY_H
#define ACTIVITY_H

#include "simmer.h"
#include "simulator.h"
#include "policy.h"

#define LABEL(name) (#name": ") << name
#define LABELC(name) LABEL(name) << ", "
#define LABELE(name) LABEL(name) << " }" << std::endl
#define C(name) name << ", "
#define E(name) name << std::endl;

/**
 *  Base class.
 */
class Activity {
public:
  BASE_CLONEABLE(Activity)

  std::string name;
  VEC<int> provide_attrs;
  int n;
  int priority;

  /**
   * Constructor.
   * @param name          the name of the activity
   * @param resource      the resource associated
   * @param provide_attrs whether the activity should expose the arrival's attributes
   * @param priority      simulation priority
   */
  Activity(std::string name, VEC<int> provide_attrs = VEC<int>(0), int priority = 0)
    : name(name), provide_attrs(provide_attrs), n(1),
      priority(priority), next(NULL), prev(NULL) {}

  Activity(const Activity& o)
    : name(o.name), provide_attrs(o.provide_attrs), n(o.n),
      priority(o.priority), next(NULL), prev(NULL) {}

  virtual ~Activity() {}

  /**
   * Print the activity info.
   * @param indent number of spaces at the beginning of each line
   */
  virtual void print(unsigned int indent = 0, bool verbose = false, bool brief = false) {
    if (!brief) {
      for (unsigned int i = 0; i < indent; ++i)
        Rcpp::Rcout << " ";
      Rcpp::Rcout << "{ Activity: " << FMT(12, left) << name << " | ";
      if (verbose) Rcpp::Rcout <<
        FMT(9, right) << prev << " <- " <<
        FMT(9, right) << this << " -> " <<
        FMT(9, left) << next << " | ";
    }
  }

  /**
   * Run the activity.
   * @param arrival a pointer to the calling arrival
   */
  virtual double run(Arrival* arrival) = 0;

  /**
   * Getter/setter for the next activity in the chain.
   */
  virtual Activity* get_next() { return next; }
  virtual void set_next(Activity* activity) { next = activity; }

  /**
   * Getter/setter for the previous activity in the chain.
   */
  virtual Activity* get_prev() { return prev; }
  virtual void set_prev(Activity* activity) { prev = activity; }

protected:
  Activity* next;
  Activity* prev;

  template <typename T>
  T get(T var, int index, Arrival* arrival) const { return var; }

  template <typename T>
  T get(Rcpp::Function call, int index, Arrival* arrival) const {
    switch (provide_attrs[index]) {
    case 1:
      return Rcpp::as<T>(call(*arrival->get_attributes()));
    case 2:
      return Rcpp::as<T>(call(*arrival->get_attributes(),
                              *arrival->sim->get_attributes()));
    default:
      return Rcpp::as<T>(call());
    }
  }
};

// abstract class for multipath activities
class Fork : public Activity {
public:
  Fork(std::string name, VEC<bool> cont, VEC<Rcpp::Environment> trj,
       VEC<int> provide_attrs = VEC<int>(0), int priority = 0)
    : Activity(name, provide_attrs, priority), cont(cont), trj(trj), selected(NULL)
  {
    foreach_ (const VEC<Rcpp::Environment>::value_type& itr, trj) {
      Rcpp::Function head(itr["head"]);
      Rcpp::Function tail(itr["tail"]);
      heads.push_back(Rcpp::as<Rcpp::XPtr<Activity> >(head()));
      tails.push_back(Rcpp::as<Rcpp::XPtr<Activity> >(tail()));
      Rcpp::Function get_n_activities(itr["get_n_activities"]);
      n += Rcpp::as<int>(get_n_activities());
    }
    foreach_ (const VEC<Activity*>::value_type& itr, heads)
      itr->set_prev(this);
  }

  Fork(const Fork& o) : Activity(o), cont(o.cont), trj(o.trj), selected(NULL) {
    heads.clear();
    tails.clear();
    foreach_ (VEC<Rcpp::Environment>::value_type& itr, trj) {
      Rcpp::Function clone(itr["clone"]);
      itr = clone();
      Rcpp::Function head(itr["head"]);
      Rcpp::Function tail(itr["tail"]);
      heads.push_back(Rcpp::as<Rcpp::XPtr<Activity> >(head()));
      tails.push_back(Rcpp::as<Rcpp::XPtr<Activity> >(tail()));
    }
    foreach_ (const VEC<Activity*>::value_type& itr, heads)
      itr->set_prev(this);
  }

  void print(unsigned int indent = 0, bool verbose = false, bool brief = false) {
    indent += 2;
    if (!brief) {
      if (indent > 10) return; // max 6 levels
      for (unsigned int i = 0; i < trj.size(); i++) {
        for (unsigned int j = 0; j < indent; ++j) Rcpp::Rcout << " ";
        Rcpp::Rcout << "Fork " << i+1 << (cont[i] ? ", continue," : ", stop,");
        Rcpp::Function print(trj[i]["print"]);
        print(indent, verbose);
      }
    } else Rcpp::Rcout << trj.size() << " paths" << std::endl;
  }

  void set_next(Activity* activity) {
    Activity::set_next(activity);
    for (unsigned int i = 0; i < tails.size(); i++) {
      if (cont[i]) tails[i]->set_next(activity);
    }
  }

  Activity* get_next() {
    if (selected) {
      Activity* aux = selected;
      selected = NULL;
      return aux;
    }
    return Activity::get_next();
  }

protected:
  VEC<bool> cont;
  VEC<Rcpp::Environment> trj;
  Activity* selected;
  VEC<Activity*> heads;
  VEC<Activity*> tails;
};

// abstract class for resource retrieval
class ResGetter {
public:
  BASE_CLONEABLE(ResGetter)

  ResGetter(std::string name, std::string resource)
    : name(name), resource(resource), id(-1) {}

protected:
  std::string name;
  std::string resource;
  int id;

  void set_id(int i) { id = std::abs(i); }

  Resource* get_resource(Arrival* arrival) const {
    Resource* selected = NULL;
    if (id < 0)
      selected = arrival->sim->get_resource(resource);
    else selected = arrival->get_selected(id);
    if (!selected)
      Rcpp::stop("%s: %s: no resource selected", arrival->name, name);
    return selected;
  }
};

/**
 * Seize a resource.
 */
template <typename T>
class Seize : public Fork, public ResGetter {
public:
  CLONEABLE(Seize<T>)

  Seize(std::string resource, T amount, int provide_attrs,
        VEC<bool> cont, VEC<Rcpp::Environment> trj, unsigned short mask)
    : Fork("Seize", cont, trj, VEC<int>(1, provide_attrs)),
      ResGetter("Seize", resource), amount(amount), mask(mask) {}

  void print(unsigned int indent = 0, bool verbose = false, bool brief = false) {
    Activity::print(indent, verbose, brief);
    if (!brief) Rcpp::Rcout << LABELC(resource) << LABELE(amount);
    else Rcpp::Rcout << C(resource) << C(amount);
    Fork::print(indent, verbose, brief);
  }

  double run(Arrival* arrival) {
    int value = std::abs(get<int>(amount, 0, arrival));
    return select_path(arrival, get_resource(arrival)->seize(arrival, value));
  }

protected:
  T amount;
  unsigned short mask;

  int select_path(Arrival* arrival, int ret) {
    switch (ret) {
    case REJECT:
      if (mask & 2) {
        ret = SUCCESS;
        if (mask & 1)
          selected = heads[1];
        else
          selected = heads[0];
      } else arrival->terminate(false);
      break;
    default:
      if (mask & 1)
        selected = heads[0];
      break;
    }
    return ret;
  }
};

/**
 * Seize a selected resource.
 */
template <typename T>
class SeizeSelected : public Seize<T> {
public:
  CLONEABLE(SeizeSelected<T>)

  SeizeSelected(int id, T amount, int provide_attrs,
                VEC<bool> cont, VEC<Rcpp::Environment> trj, unsigned short mask)
    : Seize<T>("[]", amount, provide_attrs, cont, trj, mask) { this->set_id(id); }
};

/**
 * Release a resource.
 */
template <typename T>
class Release : public Activity, public ResGetter {
public:
  CLONEABLE(Release<T>)

  Release(std::string resource, T amount, int provide_attrs)
    : Activity("Release", VEC<int>(1, provide_attrs), PRIORITY_RELEASE),
      ResGetter("Release", resource), amount(amount) {}

  void print(unsigned int indent = 0, bool verbose = false, bool brief = false) {
    Activity::print(indent, verbose, brief);
    if (!brief) Rcpp::Rcout << LABELC(resource) << LABELE(amount);
    else Rcpp::Rcout << C(resource) << E(amount);
  }

  double run(Arrival* arrival) {
    int value = std::abs(get<int>(amount, 0, arrival));
    return get_resource(arrival)->release(arrival, value);
  }

protected:
  T amount;
};

/**
 * Release a selected resource.
 */
template <typename T>
class ReleaseSelected : public Release<T> {
public:
  CLONEABLE(ReleaseSelected<T>)

  ReleaseSelected(int id, T amount, int provide_attrs)
    : Release<T>("[]", amount, provide_attrs) { this->set_id(id); }
};

/**
 * Set a resource's capacity.
 */
template <typename T>
class SetCapacity : public Activity, public ResGetter {
public:
  CLONEABLE(SetCapacity<T>)

  SetCapacity(std::string resource, T value, int provide_attrs)
    : Activity("SetCapacity", VEC<int>(1, provide_attrs)),
      ResGetter("SetCapacity", resource), value(value) {}

  void print(unsigned int indent = 0, bool verbose = false, bool brief = false) {
    Activity::print(indent, verbose, brief);
    if (!brief) Rcpp::Rcout << LABELC(resource) << LABELE(value);
    else Rcpp::Rcout << C(resource) << E(value);
  }

  double run(Arrival* arrival) {
    double ret = std::abs(get<double>(value, 0, arrival));
    if (ret == R_PosInf) ret = -1;
    get_resource(arrival)->set_capacity((int)ret);
    return 0;
  }

protected:
  T value;
};

/**
* Set a selected resource's capacity.
*/
template <typename T>
class SetCapacitySelected : public SetCapacity<T> {
public:
  CLONEABLE(SetCapacitySelected<T>)

  SetCapacitySelected(int id, T value, int provide_attrs)
    : SetCapacity<T>("[]", value, provide_attrs) { this->set_id(id); }
};

/**
* Set a resource's queue size.
*/
template <typename T>
class SetQueue : public Activity, public ResGetter {
public:
  CLONEABLE(SetQueue<T>)

  SetQueue(std::string resource, T value, int provide_attrs)
    : Activity("SetQueue", VEC<int>(1, provide_attrs)),
      ResGetter("SetQueue", resource), value(value) {}

  void print(unsigned int indent = 0, bool verbose = false, bool brief = false) {
    Activity::print(indent, verbose, brief);
    if (!brief) Rcpp::Rcout << LABELC(resource) << LABELE(value);
    else Rcpp::Rcout << C(resource) << E(value);
  }

  double run(Arrival* arrival) {
    double ret = std::abs(get<double>(value, 0, arrival));
    if (ret == R_PosInf) ret = -1;
    get_resource(arrival)->set_queue_size((int)ret);
    return 0;
  }

protected:
  T value;
};

/**
* Set a selected resource's queue size.
*/
template <typename T>
class SetQueueSelected : public SetQueue<T> {
public:
  CLONEABLE(SetQueueSelected<T>)

  SetQueueSelected(int id, T value, int provide_attrs)
    : SetQueue<T>("[]", value, provide_attrs) { this->set_id(id); }
};

/**
 * Select a resource based on some policy.
 */
template <typename T>
class Select : public Activity {
public:
  CLONEABLE(Select<T>)

  Select(T resources, int provide_attrs, std::string policy, int id)
    : Activity("Select", VEC<int>(1, provide_attrs)), resources(resources),
      id(id), policy(Policy(policy)) {}

  void print(unsigned int indent = 0, bool verbose = false, bool brief = false) {
    Activity::print(indent, verbose, brief);
    if (!brief) Rcpp::Rcout << LABELC(resources) << LABELE(policy);
    else Rcpp::Rcout << C(resources) << E(policy);
  }

  double run(Arrival* arrival) {
    VEC<std::string> res = get<VEC<std::string> >(resources, 0, arrival);
    arrival->set_selected(id, policy.dispatch(arrival->sim, res));
    return 0;
  }

protected:
  T resources;
  int id;
  Policy policy;
};

/**
 * Set attributes.
 */
template <typename T>
class SetAttribute : public Activity {
public:
  CLONEABLE(SetAttribute<T>)

  SetAttribute(std::string key, T value, int provide_attrs, bool global)
    : Activity("SetAttribute", VEC<int>(1, provide_attrs)),
      key(key), value(value), global(global) {}

  void print(unsigned int indent = 0, bool verbose = false, bool brief = false) {
    Activity::print(indent, verbose, brief);
    if (!brief) Rcpp::Rcout << LABELC(key) << LABELC(value) << LABELE(global);
    else Rcpp::Rcout << C(key) << C(value) << E(global);
  }

  double run(Arrival* arrival) {
    double ret = get<double>(value, 0, arrival);
    if (global)
      arrival->sim->set_attribute(key, ret);
    else arrival->set_attribute(key, ret);
    return 0;
  }

protected:
  std::string key;
  T value;
  bool global;
};

/**
 * Activate a generator.
 */
template <typename T>
class Activate : public Activity {
public:
  CLONEABLE(Activate<T>)

  Activate(T generator, int provide_attrs)
    : Activity("Activate", VEC<int>(1, provide_attrs), PRIORITY_MAX),
      generator(generator) {}

  void print(unsigned int indent = 0, bool verbose = false, bool brief = false) {
    Activity::print(indent, verbose, brief);
    if (!brief) Rcpp::Rcout << LABELE(generator);
    else Rcpp::Rcout << E(generator);
  }

  double run(Arrival* arrival) {
    std::string ret = get<std::string>(generator, 0, arrival);
    arrival->sim->get_generator(ret)->activate();
    return 0;
  }

protected:
  T generator;
};

/**
 * Deactivate a generator.
 */
template <typename T>
class Deactivate : public Activity {
public:
  CLONEABLE(Deactivate<T>)

  Deactivate(T generator, int provide_attrs)
    : Activity("Deactivate", VEC<int>(1, provide_attrs), PRIORITY_MAX),
      generator(generator) {}

  void print(unsigned int indent = 0, bool verbose = false, bool brief = false) {
    Activity::print(indent, verbose, brief);
    if (!brief) Rcpp::Rcout << LABELE(generator);
    else Rcpp::Rcout << E(generator);
  }

  double run(Arrival* arrival) {
    std::string ret = get<std::string>(generator, 0, arrival);
    arrival->sim->get_generator(ret)->deactivate();
    return 0;
  }

protected:
  T generator;
};

/**
 * Set a generator's trajectory.
 */
template <typename T>
class SetTraj : public Activity {
public:
  CLONEABLE(SetTraj<T>)

  SetTraj(T generator, int provide_attrs, Rcpp::Environment trajectory)
    : Activity("SetTraj", VEC<int>(1, provide_attrs), PRIORITY_MAX),
      generator(generator), trajectory(trajectory) {}

  void print(unsigned int indent = 0, bool verbose = false, bool brief = false) {
    Activity::print(indent, verbose, brief);
    if (!brief) Rcpp::Rcout << LABELC(generator) << LABELE(trajectory);
    else Rcpp::Rcout << C(generator) << E(trajectory);
  }

  double run(Arrival* arrival) {
    std::string ret = get<std::string>(generator, 0, arrival);
    arrival->sim->get_generator(ret)->set_trajectory(trajectory);
    return 0;
  }

protected:
  T generator;
  Rcpp::Environment trajectory;
};

/**
 * Set a generator's distribution.
 */
template <typename T>
class SetDist : public Activity {
public:
  CLONEABLE(SetDist<T>)

  SetDist(T generator, int provide_attrs, Rcpp::Function distribution)
    : Activity("SetDist", VEC<int>(1, provide_attrs), PRIORITY_MAX),
      generator(generator), distribution(distribution) {}

  void print(unsigned int indent = 0, bool verbose = false, bool brief = false) {
    Activity::print(indent, verbose, brief);
    if (!brief) Rcpp::Rcout << LABELC(generator) << LABELE(distribution);
    else Rcpp::Rcout << C(generator) << E(distribution);
  }

  double run(Arrival* arrival) {
    std::string ret = get<std::string>(generator, 0, arrival);
    arrival->sim->get_generator(ret)->set_distribution(distribution);
    return 0;
  }

protected:
  T generator;
  Rcpp::Function distribution;
};

/**
 * Set prioritization.
 */
template <typename T>
class SetPrior : public Activity {
public:
  CLONEABLE(SetPrior<T>)

  SetPrior(T values, int provide_attrs)
    : Activity("SetPrior", VEC<int>(1, provide_attrs)), values(values) {}

  void print(unsigned int indent = 0, bool verbose = false, bool brief = false) {
    Activity::print(indent, verbose, brief);
    if (!brief) Rcpp::Rcout << LABELE(values);
    else Rcpp::Rcout << E(values);
  }

  double run(Arrival* arrival) {
    VEC<int> ret = get<VEC<int> >(values, 0, arrival);
    if (ret.size() != 3)
      Rcpp::stop("%s: 3 values needed", name);
    if (ret[0] >= 0) arrival->order.set_priority(ret[0]);
    if (ret[1] >= 0) arrival->order.set_preemptible(ret[1]);
    if (ret[2] >= 0) arrival->order.set_restart((bool)ret[2]);
    return 0;
  }

protected:
  T values;
};

/**
 * Timeout.
 */
template <typename T>
class Timeout : public Activity {
public:
  CLONEABLE(Timeout<T>)

  Timeout(T delay, int provide_attrs)
    : Activity("Timeout", VEC<int>(1, provide_attrs)), delay(delay) {}

  void print(unsigned int indent = 0, bool verbose = false, bool brief = false) {
    Activity::print(indent, verbose, brief);
    if (!brief) Rcpp::Rcout << LABELE(delay);
    else Rcpp::Rcout << E(delay);
  }

  double run(Arrival* arrival) {
    double value = get<double>(delay, 0, arrival);
    if (ISNAN(value))
      Rcpp::stop("%s: missing value (NA or NaN returned)", name);
    return std::abs(value);
  }

protected:
  T delay;
};

/**
 * Branch. It runs as another activity, but encloses other trajectories
 * that are selected at runtime through a user-defined function.
 */
class Branch : public Fork {
public:
  CLONEABLE(Branch)

  Branch(Rcpp::Function option, int provide_attrs,
         VEC<bool> cont, VEC<Rcpp::Environment> trj)
    : Fork("Branch", cont, trj, VEC<int>(1, provide_attrs)), option(option) {}

  void print(unsigned int indent = 0, bool verbose = false, bool brief = false) {
    Activity::print(indent, verbose, brief);
    if (!brief) Rcpp::Rcout << LABELE(option);
    else Rcpp::Rcout << C(option);
    Fork::print(indent, verbose, brief);
  }

  double run(Arrival* arrival) {
    int ret = get<int>(option, 0, arrival);
    if (ret < 0 || ret > (int)heads.size())
      Rcpp::stop("%s: index out of range", name);
    if (ret) selected = heads[ret-1];
    return 0;
  }

protected:
  Rcpp::Function option;
};

/**
 * Rollback to a previous activity.
 */
class Rollback : public Activity {
public:
  CLONEABLE(Rollback)

  Rollback(int amount, int times,
           OPT<Rcpp::Function> check = NONE, int provide_attrs = false)
    : Activity("Rollback", VEC<int>(1, provide_attrs)), amount(std::abs(amount)),
      times(times), check(check), cached(NULL), selected(NULL) {}

  Rollback(const Rollback& o)
    : Activity(o), amount(o.amount), times(o.times), check(o.check),
      cached(NULL), selected(NULL)
  {
    pending.clear();
  }

  void print(unsigned int indent = 0, bool verbose = false, bool brief = false) {
    if (!cached) cached = goback();
    Activity::print(indent, verbose, brief);
    if (!brief) {
      Rcpp::Rcout << "amount: " << amount << " (" << cached->name << "), ";
      if (check) Rcpp::Rcout << LABELE(*check);
      else Rcpp::Rcout << LABELE(times);
    } else Rcpp::Rcout << cached->name << std::endl;
  }

  double run(Arrival* arrival) {
    if (check) {
      if (!get<bool>(*check, 0, arrival))
        return 0;
    } else if (times >= 0) {
      if (pending.find(arrival) == pending.end())
        pending[arrival] = times;
      if (!pending[arrival]) {
        pending.erase(arrival);
        return 0;
      }
      pending[arrival]--;
    }
    if (!cached) cached = goback();
    selected = cached;
    return 0;
  }

  Activity* get_next() {
    if (selected) {
      Activity* aux = selected;
      selected = NULL;
      return aux;
    }
    return Activity::get_next();
  }

protected:
  int amount;
  int times;
  OPT<Rcpp::Function> check;
  Activity* cached, *selected;
  UMAP<Arrival*, int> pending;

  Activity* goback() {
    int n = amount;
    Activity* ptr = this;
    while (ptr->get_prev() && n--)
      ptr = ptr->get_prev();
    return ptr;
  }
};

/**
 * Leave the trajectory with some probability.
 */
template <typename T>
class Leave : public Activity {
public:
  CLONEABLE(Leave<T>)

  Leave(T prob, int provide_attrs)
    : Activity("Leave", VEC<int>(1, provide_attrs)), prob(prob) {}

  void print(unsigned int indent = 0, bool verbose = false, bool brief = false) {
    Activity::print(indent, verbose, brief);
    if (!brief) Rcpp::Rcout << LABELE(prob);
    else Rcpp::Rcout << E(prob);
  }

  double run(Arrival* arrival) {
    if (Rcpp::runif(1)[0] > get<double>(prob, 0, arrival))
      return 0;
    arrival->terminate(false);
    return REJECT;
  }

protected:
  T prob;
};

/**
 * Clone an arrival.
 */
template <typename T>
class Clone : public Fork {
public:
  CLONEABLE(Clone<T>)

  Clone(T n, int provide_attrs, VEC<Rcpp::Environment> trj)
    : Fork("Clone", VEC<bool>(trj.size(), true),
      trj, VEC<int>(1, provide_attrs)), n(n) {}

  void print(unsigned int indent = 0, bool verbose = false, bool brief = false) {
    Activity::print(indent, verbose, brief);
    if (!brief) Rcpp::Rcout << LABELE(n);
    else Rcpp::Rcout << C(n);
    Fork::print(indent, verbose, brief);
  }

  double run(Arrival* arrival) {
    unsigned int value = std::abs(get<int>(n, 0, arrival));
    for (unsigned int i = 1; i < value; i++) {
      if (i < heads.size())
        selected = heads[i];
      Arrival* new_arrival = arrival->clone();
      new_arrival->set_activity(this->get_next());
      new_arrival->activate();
    }
    if (heads.size())
      selected = heads[0];
    return 0;
  }

protected:
  T n;
};

/**
 * Synchronize clones.
 */
class Synchronize : public Activity {
public:
  CLONEABLE(Synchronize)

  Synchronize(bool wait, bool terminate)
    : Activity("Synchronize"), wait(wait), terminate(terminate) {}

  Synchronize(const Synchronize& o)
    : Activity(o), wait(o.wait), terminate(o.terminate)
  {
    pending.clear();
  }

  void print(unsigned int indent = 0, bool verbose = false, bool brief = false) {
    Activity::print(indent, verbose, brief);
    if (!brief) Rcpp::Rcout << LABELE(wait);
    else Rcpp::Rcout << E(wait);
  }

  double run(Arrival* arrival) {
    if (!wait) {
      UMAP<std::string, int>::iterator search = pending.find(arrival->name);
      if (search == pending.end()) {
        if (*(arrival->clones) > 1)
          pending.emplace(arrival->name, *(arrival->clones)-1);
        return 0;
      } else {
        search->second--;
        if (!search->second)
          pending.erase(search);
      }
    } else if (*(arrival->clones) == 1)
      return 0;

    if (!terminate)
      delete arrival;
    else
      arrival->terminate(true);
    return REJECT;
  }

protected:
  bool wait;
  bool terminate;
  UMAP<std::string, int> pending;
};

/**
 * Create a batch.
 */
template <typename T>
class Batch : public Activity {
public:
  CLONEABLE(Batch<T>)

  Batch(int n, T timeout, VEC<int> provide_attrs, bool permanent,
        std::string id = "", OPT<Rcpp::Function> rule = NONE)
    : Activity("Batch", provide_attrs),
      n(n), timeout(timeout), permanent(permanent), id(id), rule(rule) {}

  void print(unsigned int indent = 0, bool verbose = false, bool brief = false) {
    Activity::print(indent, verbose, brief);
    if (!brief)
      Rcpp::Rcout << LABELC(n) << LABELC(timeout) << LABELC(permanent) << LABELE(id);
    else Rcpp::Rcout << C(n) << C(timeout) << C(permanent) << E(id);
  }

  double run(Arrival* arrival) {
    if (rule && !get<bool>(*rule, 1, arrival))
      return 0;
    Batched** ptr = arrival->sim->get_batch(this, id);
    if (!(*ptr))
      *ptr = init(arrival);
    (*ptr)->insert(arrival);
    if ((int)(*ptr)->size() == n)
      trigger(arrival->sim, *ptr);
    return REJECT;
  }

protected:
  int n;
  T timeout;
  bool permanent;
  std::string id;
  OPT<Rcpp::Function> rule;

  Batched* init(Arrival* arrival) {
    std::string str;
    Batched* ptr = NULL;
    if (id.size()) {
      str = "batch_" + id;
      ptr = new Batched(arrival->sim, str, permanent);
    } else {
      int count = arrival->sim->get_batch_count();
      str= "batch" + boost::lexical_cast<std::string>(count);
      ptr = new Batched(arrival->sim, str, permanent, count);
    }
    double dt = std::abs(get<double>(timeout, 0, arrival));
    if (dt) {
      Task* task = new Task(arrival->sim, "Batch-Timer",
                            boost::bind(&Batch::trigger, this, arrival->sim, ptr),
                            PRIORITY_MIN);
      task->activate(dt);
    }
    return ptr;
  }

  void trigger(Simulator* sim, Batched* target) {
    Batched** ptr = sim->get_batch(this, id);
    if (!(*ptr) || *ptr != target)
      return;
    if ((*ptr)->size()) {
      (*ptr)->set_activity(this->get_next());
      (*ptr)->activate();
      *ptr = init(*ptr);
    } else {
      delete *ptr;
      *ptr = NULL;
    }
  }
};

/**
 * Separate a batch.
 */
class Separate : public Activity {
public:
  CLONEABLE(Separate)

  Separate() : Activity("Separate") {}

  void print(unsigned int indent = 0, bool verbose = false, bool brief = false) {
    Activity::print(indent, verbose, brief);
    if (!brief) Rcpp::Rcout << " }" << std::endl;
    else Rcpp::Rcout << std::endl;
  }

  double run(Arrival* arrival) {
    Batched* batched = dynamic_cast<Batched*>(arrival);
    if (!batched || batched->is_permanent())
      return 0;
    batched->pop_all(this->get_next());
    delete batched;
    return REJECT;
  }
};

/**
 * Renege after some time.
 */
template <typename T>
class RenegeIn : public Fork {
public:
  CLONEABLE(RenegeIn<T>)

  RenegeIn(T t, int provide_attrs, VEC<Rcpp::Environment> trj)
    : Fork("RenegeIn", VEC<bool>(trj.size(), false),
      trj, VEC<int>(1, provide_attrs)), t(t) {}

  void print(unsigned int indent = 0, bool verbose = false, bool brief = false) {
    Activity::print(indent, verbose, brief);
    if (!brief) Rcpp::Rcout << LABELE(t);
    else Rcpp::Rcout << C(t);
    Fork::print(indent, verbose, brief);
  }

  double run(Arrival* arrival) {
    double ret = std::abs(get<double>(t, 0, arrival));
    Activity* next = NULL;
    if (heads.size())
      next = heads[0];
    arrival->set_renege(ret, next);
    return 0;
  }

protected:
  T t;
};

/**
 * Renege if a signal is received.
 */
template <typename T>
class RenegeIf : public Fork {
public:
  CLONEABLE(RenegeIf<T>)

  RenegeIf(T signal, int provide_attrs, VEC<Rcpp::Environment> trj)
    : Fork("RenegeIf", VEC<bool>(trj.size(), false),
      trj, VEC<int>(1, provide_attrs)), signal(signal) {}

  void print(unsigned int indent = 0, bool verbose = false, bool brief = false) {
    Activity::print(indent, verbose, brief);
    if (!brief) Rcpp::Rcout << LABELE(signal);
    else Rcpp::Rcout << C(signal);
    Fork::print(indent, verbose, brief);
  }

  double run(Arrival* arrival) {
    std::string ret = get<std::string>(signal, 0, arrival);
    Activity* next = NULL;
    if (heads.size())
      next = heads[0];
    arrival->set_renege(ret, next);
    return 0;
  }

protected:
  T signal;
};

/**
 * Abort reneging.
 */
class RenegeAbort : public Activity {
public:
  CLONEABLE(RenegeAbort)

  RenegeAbort() : Activity("RenegeAbort") {}

  void print(unsigned int indent = 0, bool verbose = false, bool brief = false) {
    Activity::print(indent, verbose, brief);
    if (!brief) Rcpp::Rcout << " }" << std::endl;
    else Rcpp::Rcout << std::endl;
  }

  double run(Arrival* arrival) {
    arrival->cancel_renege();
    return 0;
  }
};

/**
 * Send signals.
 */
template <typename T, typename U>
class Send : public Activity {
public:
  CLONEABLE(Send<T COMMA U>)

  Send(T signals, U delay, VEC<int> provide_attrs)
    : Activity("Send", provide_attrs), signals(signals), delay(delay) {}

  void print(unsigned int indent = 0, bool verbose = false, bool brief = false) {
    Activity::print(indent, verbose, brief);
    if (!brief) Rcpp::Rcout << LABELC(signals) << LABELE(delay);
    else Rcpp::Rcout << C(signals) << E(delay);
  }

  double run(Arrival* arrival) {
    VEC<std::string> sigs = get<VEC<std::string> >(signals, 0, arrival);
    double t = std::abs(get<double>(delay, 1, arrival));
    Task* task =
      new Task(arrival->sim, "Broadcast",
               boost::bind(&Simulator::broadcast, arrival->sim, sigs),
               PRIORITY_MIN);
    task->activate(t);
    return 0;
  }

protected:
  T signals;
  U delay;
};

/**
 * Subscribe to signals and assign a handler.
 */
template <typename T>
class Trap : public Fork {
public:
  CLONEABLE(Trap<T>)

  Trap(T signals, int provide_attrs,
       VEC<Rcpp::Environment> trj, bool interruptible)
    : Fork("Trap", VEC<bool>(trj.size(), false),
      trj, VEC<int>(1, provide_attrs)),
      signals(signals), interruptible(interruptible) {}

  Trap(const Trap& o) : Fork(o), signals(o.signals), interruptible(o.interruptible) {
    pending.clear();
  }

  void print(unsigned int indent = 0, bool verbose = false, bool brief = false) {
    Activity::print(indent, verbose, brief);
    if (!brief) Rcpp::Rcout << LABELE(signals);
    else Rcpp::Rcout << C(signals);
    Fork::print(indent, verbose, brief);
  }

  double run(Arrival* arrival) {
    if (!interruptible && pending.find(arrival) != pending.end()) {
      arrival->sim->subscribe(arrival);
      arrival->set_activity(pending[arrival]);
      pending.erase(arrival);
      arrival->activate();
      return REJECT;
    }
    VEC<std::string> sigs = get<VEC<std::string> >(signals, 0, arrival);
    arrival->sim->subscribe(sigs, arrival,
                            boost::bind(&Trap::launch_handler, this, arrival));
    return 0;
  }

protected:
  T signals;
  bool interruptible;
  UMAP<Arrival*, Activity*> pending;

  void launch_handler(Arrival* arrival) {
    if (!arrival->is_active())
      return;
    arrival->stop();
    if (heads.size()) {
      if (!interruptible) {
        arrival->sim->unsubscribe(arrival);
        pending[arrival] = arrival->get_activity();
        tails[0]->set_next(this);
      } else {
        tails[0]->set_next(arrival->get_activity());
      }
      arrival->set_activity(heads[0]);
    }
    arrival->activate();
  }
};

/**
 * Unsubscribe to signals.
 */
template <typename T>
class UnTrap : public Activity {
public:
  CLONEABLE(UnTrap<T>)

  UnTrap(T signals, int provide_attrs)
    : Activity("UnTrap", VEC<int>(1, provide_attrs)), signals(signals) {}

  void print(unsigned int indent = 0, bool verbose = false, bool brief = false) {
    Activity::print(indent, verbose, brief);
    if (!brief) Rcpp::Rcout << LABELE(signals);
    else Rcpp::Rcout << E(signals);
  }

  double run(Arrival* arrival) {
    VEC<std::string> sigs = get<VEC<std::string> >(signals, 0, arrival);
    arrival->sim->unsubscribe(sigs, arrival);
    return 0;
  }

protected:
  T signals;
};

/**
 * Block until a signal is received.
 */
class Wait : public Activity {
public:
  CLONEABLE(Wait)

  Wait() : Activity("Wait") {}

  void print(unsigned int indent = 0, bool verbose = false, bool brief = false) {
    Activity::print(indent, verbose, brief);
    if (!brief) Rcpp::Rcout << " }" << std::endl;
    else Rcpp::Rcout << std::endl;
  }

  double run(Arrival* arrival) { return BLOCK; }
};

/**
 * Print a message.
 */
template <typename T>
class Log : public Activity {
public:
  CLONEABLE(Log<T>)

  Log(T message, int provide_attrs)
    : Activity("Log", VEC<int>(1, provide_attrs)), message(message) {}

  void print(unsigned int indent = 0, bool verbose = false, bool brief = false) {
    Activity::print(indent, verbose, brief);
    if (!brief) Rcpp::Rcout << "message }" << std::endl;
    else Rcpp::Rcout << "message" << std::endl;
  }

  double run(Arrival* arrival) {
    Rcpp::Rcout << arrival->sim->now() << ": " << arrival->name << ": " <<
      get<std::string>(message, 0, arrival) << std::endl;
    return 0;
  }

protected:
  T message;
};

#endif
