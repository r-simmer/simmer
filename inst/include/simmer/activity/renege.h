#ifndef simmer__activity_renege_h
#define simmer__activity_renege_h

#include <simmer/activity/fork.h>

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

#endif