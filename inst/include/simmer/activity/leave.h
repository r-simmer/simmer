#ifndef simmer__activity_leave_h
#define simmer__activity_leave_h

#include <simmer/activity.h>

namespace simmer {

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

} // namespace simmer

#endif
