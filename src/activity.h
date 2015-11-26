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
  int n;
  
  /**
   * Constructor.
   * @param name      the name of the activity
   * @param resource  the resource associated
   */
  Activity(std::string name, std::string resource): 
    name(name), resource(resource), n(1), next(NULL), prev(NULL) {}
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
    Activity("Seize", resource), amount(amount) {
    if (amount < 0)
      Rcpp::stop("not allowed to seize a negative amount");
  }
  
  void show(int indent=0) {
    Activity::show(indent);
    Rcpp::Rcout << "amount: " << amount << " }" << std::endl;
  }
  
  double run(Arrival* arrival);
  
private:
  int amount;
};

/**
 * Release a resource.
 */
class Release: public Activity {
public:
  Release(std::string resource, int amount):
    Activity("Release", resource), amount(amount) {
    if (amount < 0)
      Rcpp::stop("not allowed to release a negative amount");
  }
  
  void show(int indent=0) {
    Activity::show(indent);
    Rcpp::Rcout << "amount: " << amount << " }" << std::endl;
  }
  
  double run(Arrival* arrival);
  
private:
  int amount;
};

/**
 * Set attributes.
 */
class SetAttribute: public Activity {
public:
  SetAttribute(std::string key, Rcpp::Function value, bool provide_attrs):
  Activity("SetAttribute", "none"), key(key), value(value), provide_attrs(provide_attrs) {}
  
  void show(int indent=0) {
    Activity::show(indent);
    // Rcpp::Rcout << "attr: function() }" << std::endl;
  }
  
  double run(Arrival* arrival);
  
private:
  std::string key;
  Rcpp::Function value;
  bool provide_attrs;
};

/**
 * Timeout.
 */
class Timeout: public Activity {
public:
  Timeout(Rcpp::Function duration, bool provide_attrs):
    Activity("Timeout", "none"), duration(duration), provide_attrs(provide_attrs) {}
  
  void show(int indent=0) {
    Activity::show(indent);
    Rcpp::Rcout << "duration: function() }" << std::endl;
  }
  
  double run(Arrival* arrival);
  
private:
  Rcpp::Function duration;
  bool provide_attrs;
};

/**
 * Branch. It runs as another activity, but encloses other trajectories
 * that are selected at runtime through a user-defined function.
 */
class Branch: public Activity {
public:
  Branch(Rcpp::Function option, std::vector<bool> merge, std::vector<Rcpp::Environment> trj):
    Activity("Branch", "none"), option(option), merge(merge), trj(trj), selected(NULL) {
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
  Activity("Rollback", "none"), amount(amount), times(times),
    cached(NULL), selected(NULL) {
    if (amount < 0)
      Rcpp::stop("not allowed to rollback a negative amount");
    if (times < 0)
      Rcpp::stop("not allowed to repeat a negative amount");
  }
  
  ~Rollback() { pending.clear(); }
  
  void show(int indent=0) {
    if (!cached) cached = goback();
    Activity::show(indent);
    Rcpp::Rcout << "amount: " << amount << " (" << cached->name <<
      "), times: " << times << " }" << std::endl;
  }
  
  double run(Arrival* arrival) {
    if (pending.find(arrival) == pending.end()) 
      pending[arrival] = times;
    if (!pending[arrival]) {
      pending.erase(arrival);
      return 0;
    }
    if (!cached) cached = goback();
    pending[arrival]--;
    selected = cached;
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
  int amount;
  int times;
  Activity* cached, *selected;
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
