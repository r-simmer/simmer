#ifndef ACTIVITY_H
#define ACTIVITY_H

#include <Rcpp.h>
#include <set>
#include <map>

// forward declarations
class Arrival;

/** 
 *  Base class.
 */
class Activity {
public:
  std::string name;
  std::string resource;
  bool has_func;
  bool provide_attrs;
  int n;
  
  /**
   * Constructor.
   * @param name          the name of the activity
   * @param resource      the resource associated
   * @param has_func      whether the activity stores an R function
   * @param provide_attrs whether the activity should expose the arrival's attributes
   */
  Activity(std::string name, std::string resource, bool has_func, bool provide_attrs): 
    name(name), resource(resource), has_func(has_func), provide_attrs(provide_attrs), 
    n(1), next(NULL), prev(NULL) {}
  virtual ~Activity(){}
  
  /**
   * Print the activity info.
   * @param indent number of spaces at the beginning of each line
   */
  virtual void show(int indent=0) {
    for (int i = 0; i < indent; ++i)
      Rcpp::Rcout << " ";
    Rcpp::Rcout << "{ Activity: " << name << "(" << resource << ") | ";
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
  
private:
  Activity* next;
  Activity* prev;
};

/**
 * Seize a resource.
 */
class Seize: public Activity {
public:
  Seize(std::string resource, int amount):
    Activity("Seize", resource, 0, 0), amount(fabs(amount)), amount_func("gc") {}
  Seize(std::string resource, Rcpp::Function amount, bool provide_attrs):
    Activity("Seize", resource, 1, provide_attrs), amount(-1), amount_func(amount) {}
  
  void show(int indent=0) {
    Activity::show(indent);
    if (!has_func)
      Rcpp::Rcout << "amount: " << amount << " }" << std::endl;
    else Rcpp::Rcout << "amount: function() }" << std::endl;
  }
  
  double run(Arrival* arrival);
  
private:
  int amount;
  Rcpp::Function amount_func;
};

/**
 * Release a resource.
 */
class Release: public Activity {
public:
  Release(std::string resource, int amount):
    Activity("Release", resource, 0, 0), amount(fabs(amount)), amount_func("gc") {}
  Release(std::string resource, Rcpp::Function amount, bool provide_attrs):
    Activity("Release", resource, 1, provide_attrs), amount(-1), amount_func(amount) {}
  
  void show(int indent=0) {
    Activity::show(indent);
    if (!has_func)
      Rcpp::Rcout << "amount: " << amount << " }" << std::endl;
    else Rcpp::Rcout << "amount: function() }" << std::endl;
  }
  
  double run(Arrival* arrival);
  
private:
  int amount;
  Rcpp::Function amount_func;
};

/**
 * Set attributes.
 */
class SetAttribute: public Activity {
public:
  SetAttribute(std::string key, double value):
    Activity("SetAttribute", "none", 0, 0), key(key), value(value), value_func("gc") {}
  SetAttribute(std::string key, Rcpp::Function value, bool provide_attrs):
    Activity("SetAttribute", "none", 1, provide_attrs), key(key), value(0), value_func(value) {}
  
  void show(int indent=0) {
    Activity::show(indent);
    Rcpp::Rcout << "key: " << key << ", ";
    if (!has_func)
      Rcpp::Rcout << "value: " << value << " }" << std::endl;
    else Rcpp::Rcout << "value: function() }" << std::endl;
  }
  
  double run(Arrival* arrival);
  
private:
  std::string key;
  double value;
  Rcpp::Function value_func;
};

/**
 * Timeout.
 */
class Timeout: public Activity {
public:
  Timeout(double delay):
    Activity("Timeout", "none", 0, 0), delay(fabs(delay)), task("gc") {}
  Timeout(Rcpp::Function task, bool provide_attrs):
    Activity("Timeout", "none", 1, provide_attrs), delay(-1), task(task) {}
  
  void show(int indent=0) {
    Activity::show(indent);
    if (!has_func)
      Rcpp::Rcout << "delay: " << delay << " }" << std::endl;
    else Rcpp::Rcout << "task: function() }" << std::endl;
  }
  
  double run(Arrival* arrival);
  
private:
  double delay;
  Rcpp::Function task;
};

/**
 * Branch. It runs as another activity, but encloses other trajectories
 * that are selected at runtime through a user-defined function.
 */
class Branch: public Activity {
public:
  Branch(Rcpp::Function option, std::vector<bool> merge, std::vector<Rcpp::Environment> trj):
    Activity("Branch", "none", 1, 0), option(option), merge(merge), trj(trj), selected(NULL) {
    n = 0;
    for (unsigned int i = 0; i < trj.size(); i++) {
      Rcpp::Function get_head(trj[i]["get_head"]);
      path.push_back(Rcpp::as<Rcpp::XPtr<Activity> >(get_head()));
      if (merge[i]) {
        Rcpp::Function get_tail(trj[i]["get_tail"]);
        Rcpp::XPtr<Activity> tail(get_tail());
        tail->set_next(this);
      }
      Rcpp::Function get_n_activities(trj[i]["get_n_activities"]);
      n += Rcpp::as<int>(get_n_activities());
    }
  }
  
  ~Branch() {
    merge.clear();
    path.clear();
    pending.clear();
  }
  
  void show(int indent=0) {
    for (unsigned int i = 0; i < trj.size(); i++) {
      Activity::show(indent);
      Rcpp::Rcout << "merge: " << merge[i] << " }" << std::endl;
      Rcpp::Function show(trj[i]["show"]);
      show(indent+2);
    }
  }
  
  double run(Arrival* arrival) {
    if (pending.find(arrival) != pending.end())
      pending.erase(arrival);
    else {
      unsigned int i = Rcpp::as<unsigned int>(option());
      if (i < 1 || i > path.size())
        Rcpp::stop("index out of range");
      selected = path[i-1];
      if (merge[i-1])
        pending.insert(arrival);
    }
    return 0;
  }
  
  Activity* get_next() {
    if (selected) {
      Activity* aux = selected;
      selected = NULL;
      return aux;
    } else 
      return Activity::get_next();
  }
  
private:
  Rcpp::Function option;
  std::vector<bool> merge;
  std::vector<Rcpp::Environment> trj;
  Activity* selected;
  std::vector<Activity*> path;
  std::set<Arrival*> pending;
};

/**
 * Rollback to a previous activity.
 */
class Rollback: public Activity {
public:
  Rollback(int amount, int times):
    Activity("Rollback", "none", 0, 0), amount(fabs(amount)), times(times), cached(NULL), 
    selected(NULL), check("gc") /* dirty trick */ {}
  Rollback(int amount, Rcpp::Function check, bool provide_attrs):
    Activity("Rollback", "none", 1, provide_attrs), amount(fabs(amount)), times(-1),
    cached(NULL), selected(NULL), check(check) {}
  
  ~Rollback() { pending.clear(); }
  
  void show(int indent=0) {
    if (!cached) cached = goback();
    Activity::show(indent);
    Rcpp::Rcout << "amount: " << amount << " (" << cached->name << "), ";
    if (has_func)
      Rcpp::Rcout << "check: function() }" << std::endl;
    else if (!has_func && times >= 0)
      Rcpp::Rcout << "times: " << times << " }" << std::endl;
    else
      Rcpp::Rcout << "times: Inf }" << std::endl;
  }
  
  double run(Arrival* arrival);
  
  Activity* get_next() {
    if (selected) {
      Activity* aux = selected;
      selected = NULL;
      return aux;
    } else 
      return Activity::get_next();
  }
  
private:
  int amount;
  int times;
  Activity* cached, *selected;
  Rcpp::Function check;
  std::map<Arrival*, int> pending;
  
  inline Activity* goback() {
    int n = amount;
    Activity* ptr = this;
    while (ptr->get_prev() && n--)
      ptr = ptr->get_prev();
    return ptr;
  }
};

#endif
