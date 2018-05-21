#ifndef simmer__activity_source_h
#define simmer__activity_source_h

#include <simmer/activity/activity.h>

/**
 * Activate a source.
 */
template <typename T>
class Activate : public Activity {
public:
  CLONEABLE(Activate<T>)

  Activate(const T& source)
    : Activity("Activate"), source(source) {}

  void print(unsigned int indent = 0, bool verbose = false, bool brief = false) {
    Activity::print(indent, verbose, brief);
    if (!brief) Rcpp::Rcout << LABEL1(source) << BENDL;
    else Rcpp::Rcout << BARE1(source) << ENDL;
  }

  double run(Arrival* arrival) {
    arrival->sim->get_source(get<std::string>(source, arrival))->activate();
    return 0;
  }

protected:
  T source;
};

/**
 * Deactivate a source.
 */
template <typename T>
class Deactivate : public Activity {
public:
  CLONEABLE(Deactivate<T>)

  Deactivate(const T& source)
    : Activity("Deactivate"), source(source) {}

  void print(unsigned int indent = 0, bool verbose = false, bool brief = false) {
    Activity::print(indent, verbose, brief);
    if (!brief) Rcpp::Rcout << LABEL1(source) << BENDL;
    else Rcpp::Rcout << BARE1(source) << ENDL;
  }

  double run(Arrival* arrival) {
    arrival->sim->get_source(get<std::string>(source, arrival))->deactivate();
    return 0;
  }

protected:
  T source;
};

/**
 * Set a source's source.
 */
template <typename T, typename U>
class SetSource : public Activity {
public:
  CLONEABLE(SetSource<T COMMA U>)

  SetSource(const T& source, const U& object)
    : Activity("SetSource"), source(source), object(object) {}

  void print(unsigned int indent = 0, bool verbose = false, bool brief = false) {
    Activity::print(indent, verbose, brief);
    if (!brief) Rcpp::Rcout << LABEL2(source, object) << BENDL;
    else Rcpp::Rcout << BARE2(source, object) << ENDL;
  }

  double run(Arrival* arrival) {
    arrival->sim->
      get_source(get<std::string>(source, arrival))->set_source(object);
    return 0;
  }

protected:
  T source;
  U object;
};

/**
 * Set a source's trajectory.
 */
template <typename T>
class SetTraj : public Activity {
public:
  CLONEABLE(SetTraj<T>)

  SetTraj(const T& source, const REnv& trajectory)
    : Activity("SetTraj"), source(source), trajectory(trajectory) {}

  void print(unsigned int indent = 0, bool verbose = false, bool brief = false) {
    Activity::print(indent, verbose, brief);
    if (!brief) Rcpp::Rcout << LABEL2(source, trajectory) << BENDL;
    else Rcpp::Rcout << BARE2(source, trajectory) << ENDL;
  }

  double run(Arrival* arrival) {
    arrival->sim->
      get_source(get<std::string>(source, arrival))->set_trajectory(trajectory);
    return 0;
  }

protected:
  T source;
  REnv trajectory;
};

#endif
