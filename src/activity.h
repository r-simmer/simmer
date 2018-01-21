#ifndef ACTIVITY_H
#define ACTIVITY_H

#include "simmer.h"
#include "simulator.h"
#include "policy.h"

#define SEP ", "
#define ENDL std::endl
#define BENDL " }" << ENDL

#define LABEL1(a) (#a": ") << a
#define LABEL2(a, b) LABEL1(a) << SEP << LABEL1(b)
#define LABEL3(a, b, c) LABEL1(a) << SEP << LABEL2(b, c)
#define LABEL4(a, b, c, d) LABEL1(a) << SEP << LABEL3(b, c, d)

#define BARE1(a) a
#define BARE2(a, b) BARE1(a) << SEP << BARE1(b)
#define BARE3(a, b, c) BARE1(a) << SEP << BARE2(b, c)
#define BARE4(a, b, c, d) BARE1(a) << SEP << BARE3(b, c, d)


template <typename T, typename U, typename V>
class FnWrap {
public:
  FnWrap() {}
  FnWrap(const Fn<T(U)>& call, const V& arg) : call(call), arg(arg) {}

  T operator()(U param) { return call(param); }

  friend std::ostream& operator<<(std::ostream& out, const FnWrap<T, U, V>& fn) {
    out << fn.arg;
    return out;
  }

private:
  Fn<T(U)> call;
  V arg;
};

template <typename T>
Fn<T(T, T)> get_op(char mod) {
  switch(mod) {
  case '+':
    return BIND(std::plus<double>(), _1, _2);
  case '*':
    return BIND(std::multiplies<double>(), _1, _2);
  }
  return NULL;
}

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

  ResGetter(const std::string& activity, const std::string& resource, int id = -1)
    : resource(resource), id(id), activity(activity) {}

protected:
  std::string resource;
  int id;

  Resource* get_resource(Arrival* arrival) const {
    Resource* selected = NULL;
    if (id < 0)
      selected = arrival->sim->get_resource(resource);
    else selected = arrival->get_resource_selected(id);
    if (!selected)
      Rcpp::stop("%s: %s(%s, %i): no resource selected", arrival->name, activity, resource, id);
    return selected;
  }

private:
  std::string activity;
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
    if (!brief) Rcpp::Rcout << LABEL2(resource, amount) << BENDL;
    else Rcpp::Rcout << BARE2(resource, amount) << SEP;
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
    if (!brief) Rcpp::Rcout << LABEL2(resource, amount) << BENDL;
    else Rcpp::Rcout << BARE2(resource, amount) << ENDL;
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

  SetCapacity(const std::string& resource, const T& value, char mod='N')
    : Activity("SetCapacity"), ResGetter("SetCapacity", resource),
      value(value), mod(mod), op(get_op<int>(mod)) {}

  SetCapacity(int id, const T& value, char mod='N')
    : Activity("SetCapacity"), ResGetter("SetCapacity", "[]", id),
      value(value), mod(mod), op(get_op<int>(mod)) {}

  void print(unsigned int indent = 0, bool verbose = false, bool brief = false) {
    Activity::print(indent, verbose, brief);
    if (!brief) Rcpp::Rcout << LABEL3(resource, value, mod) << BENDL;
    else Rcpp::Rcout << BARE3(resource, value, mod) << ENDL;
  }

  double run(Arrival* arrival) {
    double ret = std::abs(get<double>(value, arrival));
    int newval = (ret == R_PosInf) ? -1 : (int) ret;
    int oldval = get_resource(arrival)->get_capacity();

    if (op && newval >= 0) {
      if (oldval < 0) newval = oldval;
      else newval = (op(oldval, newval) >= 0) ? op(oldval, newval) : 0;
    }
    get_resource(arrival)->set_capacity(newval);

    if (arrival->is_paused())
      return ENQUEUE;
    return 0;
  }

protected:
  T value;
  char mod;
  Fn<int(int, int)> op;
};

/**
* Set a resource's queue size.
*/
template <typename T>
class SetQueue : public Activity, public ResGetter {
public:
  CLONEABLE(SetQueue<T>)

  SetQueue(const std::string& resource, const T& value, char mod='N')
    : Activity("SetQueue"), ResGetter("SetQueue", resource),
      value(value), mod(mod), op(get_op<int>(mod)) {}

  SetQueue(int id, const T& value, char mod='N')
    : Activity("SetQueue"), ResGetter("SetQueue", "[]", id),
      value(value), mod(mod), op(get_op<int>(mod)) {}

  void print(unsigned int indent = 0, bool verbose = false, bool brief = false) {
    Activity::print(indent, verbose, brief);
    if (!brief) Rcpp::Rcout << LABEL3(resource, value, mod) << BENDL;
    else Rcpp::Rcout << BARE3(resource, value, mod) << ENDL;
  }

  double run(Arrival* arrival) {
    double ret = std::abs(get<double>(value, arrival));
    int newval = (ret == R_PosInf) ? -1 : (int) ret;
    int oldval = get_resource(arrival)->get_queue_size();

    if (op && newval >= 0) {
      if (oldval < 0) newval = oldval;
      else newval = (op(oldval, newval) >= 0) ? op(oldval, newval) : 0;
    }
    get_resource(arrival)->set_queue_size(newval);

    return 0;
  }

protected:
  T value;
  char mod;
  Fn<int(int, int)> op;
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
    if (!brief) Rcpp::Rcout << LABEL2(resources, policy) << BENDL;
    else Rcpp::Rcout << BARE2(resources, policy) << ENDL;
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

  SetAttribute(const T& keys, const U& values, bool global, char mod='N')
    : Activity("SetAttribute"), keys(keys), values(values),
      global(global), mod(mod), op(get_op<double>(mod)) {}

  void print(unsigned int indent = 0, bool verbose = false, bool brief = false) {
    Activity::print(indent, verbose, brief);
    if (!brief) Rcpp::Rcout << LABEL4(keys, values, global, mod) << BENDL;
    else Rcpp::Rcout << BARE4(keys, values, global, mod) << ENDL;
  }

  double run(Arrival* arrival) {
    VEC<std::string> ks = get<VEC<std::string> >(keys, arrival);
    VEC<double> vals = get<VEC<double> >(values, arrival);

    if (ks.size() != vals.size())
      Rcpp::stop("%s: number of keys and values don't match", name);

    if (op) {
      for (unsigned int i = 0; i < ks.size(); i++) {
        double attr = arrival->get_attribute(ks[i], global);
        arrival->set_attribute(ks[i], op(attr, vals[i]), global);
      }
    } else for (unsigned int i = 0; i < ks.size(); i++)
      arrival->set_attribute(ks[i], vals[i], global);

    return 0;
  }

protected:
  T keys;
  U values;
  bool global;
  char mod;
  Fn<double(double, double)> op;
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
    if (!brief) Rcpp::Rcout << LABEL1(generator) << BENDL;
    else Rcpp::Rcout << BARE1(generator) << ENDL;
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
    if (!brief) Rcpp::Rcout << LABEL1(generator) << BENDL;
    else Rcpp::Rcout << BARE1(generator) << ENDL;
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
    if (!brief) Rcpp::Rcout << LABEL2(generator, trajectory) << BENDL;
    else Rcpp::Rcout << BARE2(generator, trajectory) << ENDL;
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
    if (!brief) Rcpp::Rcout << LABEL2(generator, distribution) << BENDL;
    else Rcpp::Rcout << BARE2(generator, distribution) << ENDL;
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

  SetPrior(const T& values, char mod='N')
    : Activity("SetPrior"), values(values), mod(mod), op(get_op<int>(mod)) {}

  void print(unsigned int indent = 0, bool verbose = false, bool brief = false) {
    Activity::print(indent, verbose, brief);
    if (!brief) Rcpp::Rcout << LABEL2(values, mod) << BENDL;
    else Rcpp::Rcout << BARE2(values, mod) << ENDL;
  }

  double run(Arrival* arrival) {
    VEC<int> ret = get<VEC<int> >(values, arrival);
    if (ret.size() != 3)
      Rcpp::stop("%s: 3 values needed", name);

    if (op) {
      ret[0] = op(arrival->order.get_priority(), ret[0]);
      ret[1] = op(arrival->order.get_preemptible(), ret[1]);
      ret[2] = op((int)arrival->order.get_restart(), ret[2]);
    }
    if (ret[0] >= 0) arrival->order.set_priority(ret[0]);
    if (ret[1] >= 0) arrival->order.set_preemptible(ret[1]);
    if (ret[2] >= 0) arrival->order.set_restart((bool)ret[2]);

    return 0;
  }

protected:
  T values;
  char mod;
  Fn<int(int, int)> op;
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
    if (!brief) Rcpp::Rcout << LABEL1(delay) << BENDL;
    else Rcpp::Rcout << BARE1(delay) << ENDL;
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
    if (!brief) Rcpp::Rcout << LABEL1(option) << BENDL;
    else Rcpp::Rcout << BARE1(option) << SEP;
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
      Rcpp::Rcout << LABEL1(amount) << " (" << cached->name << "), ";
      if (check) Rcpp::Rcout << LABEL1(*check) << BENDL;
      else Rcpp::Rcout << LABEL1(times) << BENDL;
    } else Rcpp::Rcout << BARE1(cached->name) << ENDL;
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
    if (!brief) Rcpp::Rcout << LABEL1(prob) << BENDL;
    else Rcpp::Rcout << BARE1(prob) << ENDL;
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
    if (!brief) Rcpp::Rcout << LABEL1(n) << BENDL;
    else Rcpp::Rcout << BARE1(n) << SEP;
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
    if (!brief) Rcpp::Rcout << LABEL1(wait) << BENDL;
    else Rcpp::Rcout << BARE1(wait) << ENDL;
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
      Rcpp::Rcout << LABEL4(n, timeout, permanent, id) << BENDL;
    else Rcpp::Rcout << BARE4(n, timeout, permanent, id) << ENDL;
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
    if (!brief) Rcpp::Rcout << BENDL;
    else Rcpp::Rcout << ENDL;
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
    if (!brief) Rcpp::Rcout << LABEL1(t) << BENDL;
    else Rcpp::Rcout << BARE1(t) << SEP;
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
    if (!brief) Rcpp::Rcout << LABEL1(signal) << BENDL;
    else Rcpp::Rcout << BARE1(signal) << SEP;
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
    if (!brief) Rcpp::Rcout << BENDL;
    else Rcpp::Rcout << ENDL;
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
    if (!brief) Rcpp::Rcout << LABEL2(signals, delay) << BENDL;
    else Rcpp::Rcout << BARE2(signals, delay) << ENDL;
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
    if (!brief) Rcpp::Rcout << LABEL1(signals) << BENDL;
    else Rcpp::Rcout << BARE1(signals) << SEP;
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
    if (!brief) Rcpp::Rcout << LABEL1(signals) << BENDL;
    else Rcpp::Rcout << BARE1(signals) << ENDL;
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
    if (!brief) Rcpp::Rcout << BENDL;
    else Rcpp::Rcout << ENDL;
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
    if (!brief) Rcpp::Rcout << "message" << BENDL;
    else Rcpp::Rcout << "message" << ENDL;
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
