#ifndef simmer__process_generator_h
#define simmer__process_generator_h

#include <simmer/process/source.h>
#include <simmer/activity.h>

/**
 * Generation process.
 */
class Generator : public Source {
public:
  Generator(Simulator* sim, const std::string& name_prefix, int mon,
            const REnv& trj, const RFn& dist, const Order& order)
    : Source(sim, name_prefix, mon, trj, order), source(dist) {}

  void reset() {
    Source::reset();
    RFn reset_fun(source.attr("reset"));
    reset_fun();
  }

  void set_source(const ANY& new_source) {
    if (new_source.type() != typeid(RFn))
      Rcpp::stop("source '%s': requires a function", name);
    source = boost::any_cast<RFn>(new_source);
  }

  void run() {
    // get the delay for the next (n) arrival(s)
    RNum delays = source();
    size_t n = delays.size();
    double delay = 0;

    for (size_t i = 0; i < n; ++i) {
      if (delays[i] < 0)
        return;
      delay += delays[i];

      // schedule the arrival
      sim->schedule(delay, new_arrival(delay),
                    first_activity->priority ? first_activity->priority : count);
    }
    // schedule the generator
    sim->schedule(delay, this, priority);
  }

private:
  RFn source;
};

#endif
