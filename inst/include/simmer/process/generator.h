#ifndef simmer__process_generator_h
#define simmer__process_generator_h

#include <simmer/process/source.h>

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

  void run();

private:
  RFn source;
};

#endif
