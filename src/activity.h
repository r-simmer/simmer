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
  int count;
  int priority;

  /**
   * Constructor.
   * @param name          the name of the activity
   * @param resource      the resource associated
   * @param priority      simulation priority
   */
  Activity(const std::string& name, int priority = 0)
    : name(name), count(1), priority(priority), next(NULL), prev(NULL) {}

  Activity(const Activity& o)
    : name(o.name), count(o.count), priority(o.priority), next(NULL), prev(NULL) {}

  virtual ~Activity() {}

  /**
   * Print the activity info.
   * @param indent number of spaces at the beginning of each line
   */
  virtual void print(unsigned int indent = 0, bool verbose = false, bool brief = false) {
    if (!brief) {
      std::ios::fmtflags fmt(Rcpp::Rcout.flags());
      Rcpp::Rcout <<
        IND(indent) << "{ Activity: " << FMT(12, left) << name << " | ";
      if (verbose) Rcpp::Rcout <<
        FMT(9, right) << prev << " <- " <<
        FMT(9, right) << this << " -> " <<
        FMT(9, left) << next << " | ";
      Rcpp::Rcout.flags(fmt);
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
  T get(const T& var, Arrival* arrival) const { return var; }

  template <typename T>
  T get(const RFn& call, Arrival* arrival) const { return Rcpp::as<T>(call()); }

  template <typename T>
  T get(const Fn<T(Arrival*)>& call, Arrival* arrival) const { return call(arrival); }
};

// abstract class for multipath activities
class Fork : public Activity {
public:
  Fork(const std::string& name, const VEC<bool>& cont,
       const VEC<REnv>& trj, int priority = 0)
    : Activity(name, priority), cont(cont), trj(trj), selected(NULL)
  {
    foreach_ (const VEC<REnv>::value_type& itr, trj) {
      RFn head(itr["head"]);
      RFn tail(itr["tail"]);
      heads.push_back(Rcpp::as<Rcpp::XPtr<Activity> >(head()));
      tails.push_back(Rcpp::as<Rcpp::XPtr<Activity> >(tail()));
      RFn get_n_activities(itr["get_n_activities"]);
      count += Rcpp::as<int>(get_n_activities());
    }
    foreach_ (const VEC<Activity*>::value_type& itr, heads)
      itr->set_prev(this);
  }

  Fork(const Fork& o) : Activity(o), cont(o.cont), trj(o.trj), selected(NULL) {
    heads.clear();
    tails.clear();
    foreach_ (VEC<REnv>::value_type& itr, trj) {
      RFn clone(itr["clone"]);
      itr = clone();
      RFn head(itr["head"]);
      RFn tail(itr["tail"]);
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
        Rcpp::Rcout <<
          IND(indent) << "Fork " << i+1 << (cont[i] ? ", continue," : ", stop,");
        RFn print(trj[i]["print"]);
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
  VEC<REnv> trj;
  Activity* selected;
  VEC<Activity*> heads;
  VEC<Activity*> tails;
};

// abstract class for resource retrieval
class ResGetter {
public:
  BASE_CLONEABLE(ResGetter)

  ResGetter(const std::string& name, const std::string& resource, int id = -1)
    : name(name), resource(resource), id(id) {}

protected:
  std::string name;
  std::string resource;
  int id;

  Resource* get_resource(Arrival* arrival) const {
    Resource* selected = NULL;
    if (id < 0)
      selected = arrival->sim->get_resource(resource);
    else selected = arrival->get_resource_selected(id);
    if (!selected)
      Rcpp::stop("%s: %s(%s, %i): no resource selected", arrival->name, name, resource, id);
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

  Seize(const std::string& resource, const T& amount, const VEC<bool>& cont,
        const VEC<REnv>& trj, unsigned short mask)
    : Fork("Seize", cont, trj),
      ResGetter("Seize", resource), amount(amount), mask(mask) {}

  Seize(int id, const T& amount, const VEC<bool>& cont,
        const VEC<REnv>& trj, unsigned short mask)
    : Fork("Seize", cont, trj),
      ResGetter("Seize", "[]", id), amount(amount), mask(mask) {}

  void print(unsigned int indent = 0, bool verbose = false, bool brief = false) {
    Activity::print(indent, verbose, brief);
    if (!brief) Rcpp::Rcout << LABELC(resource) << LABELE(amount);
    else Rcpp::Rcout << C(resource) << C(amount);
    Fork::print(indent, verbose, brief);
  }

  double run(Arrival* arrival) {
    return select_path(
      arrival, get_resource(arrival)->seize(arrival, std::abs(get<int>(amount, arrival))));
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
 * Release a resource.
 */
template <typename T>
class Release : public Activity, public ResGetter {
public:
  CLONEABLE(Release<T>)

  Release(const std::string& resource, const T& amount)
    : Activity("Release", PRIORITY_RELEASE),
      ResGetter("Release", resource), amount(amount) {}

  Release(int id, const T& amount)
    : Activity("Release", PRIORITY_RELEASE),
      ResGetter("Release", "[]", id), amount(amount) {}

  void print(unsigned int indent = 0, bool verbose = false, bool brief = false) {
    Activity::print(indent, verbose, brief);
    if (!brief) Rcpp::Rcout << LABELC(resource) << LABELE(amount);
    else Rcpp::Rcout << C(resource) << E(amount);
  }

  double run(Arrival* arrival) {
    return get_resource(arrival)->release(arrival, std::abs(get<int>(amount, arrival)));
  }

protected:
  T amount;
};

/**
 * Set a resource's capacity.
 */
template <typename T>
class SetCapacity : public Activity, public ResGetter {
public:
  CLONEABLE(SetCapacity<T>)

  SetCapacity(const std::string& resource, const T& value)
    : Activity("SetCapacity"), ResGetter("SetCapacity", resource), value(value) {}

  SetCapacity(int id, const T& value)
    : Activity("SetCapacity"), ResGetter("SetCapacity", "[]", id), value(value) {}

  void print(unsigned int indent = 0, bool verbose = false, bool brief = false) {
    Activity::print(indent, verbose, brief);
    if (!brief) Rcpp::Rcout << LABELC(resource) << LABELE(value);
    else Rcpp::Rcout << C(resource) << E(value);
  }

  double run(Arrival* arrival) {
    double ret = std::abs(get<double>(value, arrival));
    if (ret == R_PosInf) ret = -1;
    get_resource(arrival)->set_capacity((int)ret);
    if (arrival->is_paused())
      return ENQUEUE;
    return 0;
  }

protected:
  T value;
};

/**
* Set a resource's queue size.
*/
template <typename T>
class SetQueue : public Activity, public ResGetter {
public:
  CLONEABLE(SetQueue<T>)

  SetQueue(const std::string& resource, const T& value)
    : Activity("SetQueue"), ResGetter("SetQueue", resource), value(value) {}

  SetQueue(int id, const T& value)
    : Activity("SetQueue"), ResGetter("SetQueue", "[]", id), value(value) {}

  void print(unsigned int indent = 0, bool verbose = false, bool brief = false) {
    Activity::print(indent, verbose, brief);
    if (!brief) Rcpp::Rcout << LABELC(resource) << LABELE(value);
    else Rcpp::Rcout << C(resource) << E(value);
  }

  double run(Arrival* arrival) {
    double ret = std::abs(get<double>(value, arrival));
    if (ret == R_PosInf) ret = -1;
    get_resource(arrival)->set_queue_size((int)ret);
    return 0;
  }

protected:
  T value;
};

/**
 * Select a resource based on some policy.
 */
template <typename T>
class Select : public Activity {
public:
  CLONEABLE(Select<T>)

  Select(const T& resources, const std::string& policy, int id)
    : Activity("Select"), resources(resources), id(id), policy(Policy(policy)) {}

  void print(unsigned int indent = 0, bool verbose = false, bool brief = false) {
    Activity::print(indent, verbose, brief);
    if (!brief) Rcpp::Rcout << LABELC(resources) << LABELE(policy);
    else Rcpp::Rcout << C(resources) << E(policy);
  }

  double run(Arrival* arrival) {
    arrival->set_resource_selected(
        id, policy.dispatch(arrival->sim, get<VEC<std::string> >(resources, arrival)));
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
template <typename T, typename U>
class SetAttribute : public Activity {
public:
  CLONEABLE(SetAttribute<T COMMA U>)

  SetAttribute(const T& keys, const U& values, bool global)
    : Activity("SetAttribute"), keys(keys), values(values), global(global) {}

  void print(unsigned int indent = 0, bool verbose = false, bool brief = false) {
    Activity::print(indent, verbose, brief);
    if (!brief) Rcpp::Rcout << LABELC(keys) << LABELC(values) << LABELE(global);
    else Rcpp::Rcout << C(keys) << C(values) << E(global);
  }

  double run(Arrival* arrival) {
    VEC<std::string> ks = get<VEC<std::string> >(keys, arrival);
    VEC<double> vals = get<VEC<double> >(values, arrival);

    if (ks.size() != vals.size())
      Rcpp::stop("%s: number of keys and values don't match", name);

    Fn<void(const std::string&, double)> setter;
    if (global) setter = BIND(&Simulator::set_attribute, arrival->sim, _1, _2);
    else setter = BIND(&Arrival::set_attribute, arrival, _1, _2);

    for (unsigned int i = 0; i < ks.size(); i++)
      setter(ks[i], vals[i]);

    return 0;
  }

protected:
  T keys;
  U values;
  bool global;
};

/**
 * Activate a generator.
 */
template <typename T>
class Activate : public Activity {
public:
  CLONEABLE(Activate<T>)

  Activate(const T& generator)
    : Activity("Activate"), generator(generator) {}

  void print(unsigned int indent = 0, bool verbose = false, bool brief = false) {
    Activity::print(indent, verbose, brief);
    if (!brief) Rcpp::Rcout << LABELE(generator);
    else Rcpp::Rcout << E(generator);
  }

  double run(Arrival* arrival) {
    arrival->sim->get_generator(get<std::string>(generator, arrival))->activate();
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

  Deactivate(const T& generator)
    : Activity("Deactivate"), generator(generator) {}

  void print(unsigned int indent = 0, bool verbose = false, bool brief = false) {
    Activity::print(indent, verbose, brief);
    if (!brief) Rcpp::Rcout << LABELE(generator);
    else Rcpp::Rcout << E(generator);
  }

  double run(Arrival* arrival) {
    arrival->sim->get_generator(get<std::string>(generator, arrival))->deactivate();
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

  SetTraj(const T& generator, const REnv& trajectory)
    : Activity("SetTraj"), generator(generator), trajectory(trajectory) {}

  void print(unsigned int indent = 0, bool verbose = false, bool brief = false) {
    Activity::print(indent, verbose, brief);
    if (!brief) Rcpp::Rcout << LABELC(generator) << LABELE(trajectory);
    else Rcpp::Rcout << C(generator) << E(trajectory);
  }

  double run(Arrival* arrival) {
    arrival->sim->
      get_generator(get<std::string>(generator, arrival))->set_trajectory(trajectory);
    return 0;
  }

protected:
  T generator;
  REnv trajectory;
};

/**
 * Set a generator's distribution.
 */
template <typename T>
class SetDist : public Activity {
public:
  CLONEABLE(SetDist<T>)

  SetDist(const T& generator, const RFn& distribution)
    : Activity("SetDist"), generator(generator), distribution(distribution) {}

  void print(unsigned int indent = 0, bool verbose = false, bool brief = false) {
    Activity::print(indent, verbose, brief);
    if (!brief) Rcpp::Rcout << LABELC(generator) << LABELE(distribution);
    else Rcpp::Rcout << C(generator) << E(distribution);
  }

  double run(Arrival* arrival) {
    arrival->sim->
      get_generator(get<std::string>(generator, arrival))->set_distribution(distribution);
    return 0;
  }

protected:
  T generator;
  RFn distribution;
};

/**
 * Set prioritization.
 */
template <typename T>
class SetPrior : public Activity {
public:
  CLONEABLE(SetPrior<T>)

  SetPrior(const T& values) : Activity("SetPrior"), values(values) {}

  void print(unsigned int indent = 0, bool verbose = false, bool brief = false) {
    Activity::print(indent, verbose, brief);
    if (!brief) Rcpp::Rcout << LABELE(values);
    else Rcpp::Rcout << E(values);
  }

  double run(Arrival* arrival) {
    VEC<int> ret = get<VEC<int> >(values, arrival);
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

  Timeout(const T& delay) : Activity("Timeout"), delay(delay) {}

  void print(unsigned int indent = 0, bool verbose = false, bool brief = false) {
    Activity::print(indent, verbose, brief);
    if (!brief) Rcpp::Rcout << LABELE(delay);
    else Rcpp::Rcout << E(delay);
  }

  double run(Arrival* arrival) {
    double value = get<double>(delay, arrival);
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

  Branch(const RFn& option, const VEC<bool>& cont, const VEC<REnv>& trj)
    : Fork("Branch", cont, trj), option(option) {}

  void print(unsigned int indent = 0, bool verbose = false, bool brief = false) {
    Activity::print(indent, verbose, brief);
    if (!brief) Rcpp::Rcout << LABELE(option);
    else Rcpp::Rcout << C(option);
    Fork::print(indent, verbose, brief);
  }

  double run(Arrival* arrival) {
    int ret = get<int>(option, arrival);
    if (ret < 0 || ret > (int)heads.size())
      Rcpp::stop("%s: index out of range", name);
    if (ret) selected = heads[ret-1];
    return 0;
  }

protected:
  RFn option;
};

/**
 * Rollback to a previous activity.
 */
class Rollback : public Activity {
public:
  CLONEABLE(Rollback)

  Rollback(int amount, int times, const OPT<RFn>& check = NONE)
    : Activity("Rollback"), amount(std::abs(amount)),
      times(times), check(check), cached(NULL), selected(NULL) {}

  Rollback(const Rollback& o)
    : Activity(o), amount(o.amount), times(o.times), check(o.check),
      cached(NULL), selected(NULL) { pending.clear(); }

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
      if (!get<bool>(*check, arrival))
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
  OPT<RFn> check;
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

  Leave(const T& prob) : Activity("Leave"), prob(prob) {}

  void print(unsigned int indent = 0, bool verbose = false, bool brief = false) {
    Activity::print(indent, verbose, brief);
    if (!brief) Rcpp::Rcout << LABELE(prob);
    else Rcpp::Rcout << E(prob);
  }

  double run(Arrival* arrival) {
    if (Rcpp::runif(1)[0] > get<double>(prob, arrival))
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

  Clone(const T& n, const VEC<REnv>& trj)
    : Fork("Clone", VEC<bool>(trj.size(), true), trj), n(n) {}

  void print(unsigned int indent = 0, bool verbose = false, bool brief = false) {
    Activity::print(indent, verbose, brief);
    if (!brief) Rcpp::Rcout << LABELE(n);
    else Rcpp::Rcout << C(n);
    Fork::print(indent, verbose, brief);
  }

  double run(Arrival* arrival) {
    for (unsigned int i = 1; i < std::abs(get<int>(n, arrival)); i++) {
      if (i < heads.size())
        selected = heads[i];
      Arrival* new_arrival = arrival->clone();
      new_arrival->set_activity(get_next());
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
    : Activity(o), wait(o.wait), terminate(o.terminate) { pending.clear(); }

  void print(unsigned int indent = 0, bool verbose = false, bool brief = false) {
    Activity::print(indent, verbose, brief);
    if (!brief) Rcpp::Rcout << LABELE(wait);
    else Rcpp::Rcout << E(wait);
  }

  double run(Arrival* arrival) {
    if (!wait) {
      UMAP<std::string, int>::iterator search = pending.find(arrival->name);
      if (search == pending.end()) {
        if (arrival->get_clones() > 1)
          pending.emplace(arrival->name, arrival->get_clones()-1);
        return 0;
      } else {
        search->second--;
        if (!search->second)
          pending.erase(search);
      }
    } else if (arrival->get_clones() == 1)
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

  Batch(int n, const T& timeout, bool permanent,
        const std::string& id = "", const OPT<RFn>& rule = NONE)
    : Activity("Batch"),
      n(n), timeout(timeout), permanent(permanent), id(id), rule(rule) {}

  void print(unsigned int indent = 0, bool verbose = false, bool brief = false) {
    Activity::print(indent, verbose, brief);
    if (!brief)
      Rcpp::Rcout << LABELC(n) << LABELC(timeout) << LABELC(permanent) << LABELE(id);
    else Rcpp::Rcout << C(n) << C(timeout) << C(permanent) << E(id);
  }

  double run(Arrival* arrival) {
    if (rule && !get<bool>(*rule, arrival))
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
  OPT<RFn> rule;

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
    double dt = std::abs(get<double>(timeout, arrival));
    if (dt) {
      Task* task = new Task(arrival->sim, "Batch-Timer",
                            BIND(&Batch::trigger, this, arrival->sim, ptr),
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
      (*ptr)->set_activity(get_next());
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
    batched->pop_all(get_next());
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

  RenegeIn(const T& t, const VEC<REnv>& trj)
    : Fork("RenegeIn", VEC<bool>(trj.size(), false), trj), t(t) {}

  void print(unsigned int indent = 0, bool verbose = false, bool brief = false) {
    Activity::print(indent, verbose, brief);
    if (!brief) Rcpp::Rcout << LABELE(t);
    else Rcpp::Rcout << C(t);
    Fork::print(indent, verbose, brief);
  }

  double run(Arrival* arrival) {
    Activity* next = NULL;
    if (heads.size())
      next = heads[0];
    arrival->set_renege(std::abs(get<double>(t, arrival)), next);
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

  RenegeIf(const T& signal, const VEC<REnv>& trj)
    : Fork("RenegeIf", VEC<bool>(trj.size(), false), trj), signal(signal) {}

  void print(unsigned int indent = 0, bool verbose = false, bool brief = false) {
    Activity::print(indent, verbose, brief);
    if (!brief) Rcpp::Rcout << LABELE(signal);
    else Rcpp::Rcout << C(signal);
    Fork::print(indent, verbose, brief);
  }

  double run(Arrival* arrival) {
    Activity* next = NULL;
    if (heads.size())
      next = heads[0];
    arrival->set_renege(get<std::string>(signal, arrival), next);
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

  Send(const T& signals, const U& delay)
    : Activity("Send", PRIORITY_SEND), signals(signals), delay(delay) {}

  void print(unsigned int indent = 0, bool verbose = false, bool brief = false) {
    Activity::print(indent, verbose, brief);
    if (!brief) Rcpp::Rcout << LABELC(signals) << LABELE(delay);
    else Rcpp::Rcout << C(signals) << E(delay);
  }

  double run(Arrival* arrival) {
    double lag = std::abs(get<double>(delay, arrival));
    Task* task =
      new Task(arrival->sim, "Broadcast",
               BIND(&Simulator::broadcast, arrival->sim,
                    get<VEC<std::string> >(signals, arrival)),
               lag ? PRIORITY_MIN : PRIORITY_SIGNAL);
    task->activate(lag);
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

  Trap(const T& signals, const VEC<REnv>& trj, bool interruptible)
    : Fork("Trap", VEC<bool>(trj.size(), false), trj, PRIORITY_TRAP),
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
    arrival->sim->subscribe(get<VEC<std::string> >(signals, arrival), arrival,
                            BIND(&Trap::launch_handler, this, arrival));
    return 0;
  }

protected:
  T signals;
  bool interruptible;
  UMAP<Arrival*, Activity*> pending;

  void launch_handler(Arrival* arrival) {
    if (!arrival->sim->is_scheduled(arrival))
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

  UnTrap(const T& signals) : Activity("UnTrap", PRIORITY_TRAP), signals(signals) {}

  void print(unsigned int indent = 0, bool verbose = false, bool brief = false) {
    Activity::print(indent, verbose, brief);
    if (!brief) Rcpp::Rcout << LABELE(signals);
    else Rcpp::Rcout << E(signals);
  }

  double run(Arrival* arrival) {
    arrival->sim->unsubscribe(get<VEC<std::string> >(signals, arrival), arrival);
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

  Log(const T& message) : Activity("Log"), message(message) {}

  void print(unsigned int indent = 0, bool verbose = false, bool brief = false) {
    Activity::print(indent, verbose, brief);
    if (!brief) Rcpp::Rcout << "message }" << std::endl;
    else Rcpp::Rcout << "message" << std::endl;
  }

  double run(Arrival* arrival) {
    Rcpp::Rcout << arrival->sim->now() << ": " << arrival->name << ": " <<
      get<std::string>(message, arrival) << std::endl;
    return 0;
  }

protected:
  T message;
};

#endif
