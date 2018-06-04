#ifndef simmer__activity_branch_h
#define simmer__activity_branch_h

#include <simmer/activity.h>
#include <simmer/activity/fork.h>

namespace simmer {

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
      internal::print(brief, false, ARG(option));
      Fork::print(indent, verbose, brief);
    }

    double run(Arrival* arrival) {
      int ret = get<int>(option, arrival);
      if (ret < 0 || ret > (int)heads.size())
        Rcpp::stop("index out of range");
      if (ret) path = ret-1;
      return 0;
    }

  protected:
    RFn option;
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
      internal::print(brief, false, ARG(n));
      Fork::print(indent, verbose, brief);
    }

    double run(Arrival* arrival) {
      unsigned int ret = (unsigned int) std::abs(get<int>(n, arrival));
      for (unsigned int i = 1; i < ret; i++) {
        if (i < heads.size())
          path = i;
        Arrival* new_arrival = arrival->clone();
        new_arrival->set_activity(get_next());
        new_arrival->activate();
      }
      if (heads.size())
        path = 0;
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
      internal::print(brief, true, ARG(wait));
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

} // namespace simmer

#endif
