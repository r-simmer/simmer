#ifndef simmer__activity_fork_h
#define simmer__activity_fork_h

#include <simmer/activity/activity.h>

// abstract class for multipath activities
class Fork : public Activity {
public:
  Fork(const std::string& name, const VEC<bool>& cont,
       const VEC<REnv>& trj, int priority = 0)
    : Activity(name, priority), cont(cont), trj(trj), selected(NULL)
  {
    foreach_ (const VEC<REnv>::value_type& itr, trj) {
      heads.push_back(trj_get(itr, "head"));
      tails.push_back(trj_get(itr, "tail"));
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
      heads.push_back(trj_get(itr, "head"));
      tails.push_back(trj_get(itr, "tail"));
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

#endif
