#ifndef simmer__activity_log_h
#define simmer__activity_log_h

#include <simmer/activity.h>
#include <simmer/process/arrival.h>

namespace simmer {

  /**
   * Print a message.
   */
  template <typename T>
  class Log : public Activity {
  public:
    CLONEABLE(Log<T>)

    Log(const T& message, int level)
      : Activity("Log"), message(message), level(level) {}

    void print(unsigned int indent = 0, bool verbose = false, bool brief = false) {
      Activity::print(indent, verbose, brief);
      internal::print(brief, true, "message", "message", ARG(level));
    }

    double run(Arrival* arrival) {
      int log_level = arrival->sim->log_level;
      if (log_level < 0 || (level >= 0 && log_level >= level))
        Rcpp::Rcout << arrival->sim->now() << ": " << arrival->name << ": " <<
          get<std::string>(message, arrival) << std::endl;
      return 0;
    }

  protected:
    T message;
    int level;
  };

} // namespace simmer

#endif
