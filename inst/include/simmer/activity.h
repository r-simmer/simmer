#ifndef simmer__activity_h
#define simmer__activity_h

#include <simmer/common.h>
#include <simmer/simulator.h>
#include <simmer/activity/utils/macros.h>

namespace simmer {

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

  inline Activity* trj_get(const REnv& trj, const std::string& method) {
    RFn func(trj[method]);
    return Rcpp::as<Rcpp::XPtr<Activity> >(func());
  }

} // namespace simmer

#endif
