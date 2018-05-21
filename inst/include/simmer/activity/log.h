#ifndef simmer__activity_log_h
#define simmer__activity_log_h

#include <simmer/activity/activity.h>

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
