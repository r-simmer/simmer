#ifndef simmer__activity_timeout_h
#define simmer__activity_timeout_h

#include <simmer/activity.h>

namespace simmer {

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
      internal::print(brief, true, ARG(delay));
    }

    double run(Arrival* arrival) {
      double value = get<double>(delay, arrival);
      if (ISNAN(value))
        Rcpp::stop("missing value (NA or NaN returned)");
      return std::abs(value);
    }

  protected:
    T delay;
  };

} // namespace simmer

#endif
