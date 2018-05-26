#ifndef simmer__process_manager_h
#define simmer__process_manager_h

#include <simmer/process.h>

namespace simmer {

  class Manager : public Process {
    typedef Fn<void(int)> Setter;

  public:
    Manager(Simulator* sim, const std::string& name, const std::string& param,
            const VEC<double>& duration, const VEC<int>& value, int period, const Setter& set)
      : Process(sim, name, false, PRIORITY_MANAGER), param(param),
        duration(duration), value(value), period(period), set(set), index(0) {}

    void reset() { index = 0; }

    void run() {
      if (sim->verbose) Rcpp::Rcout <<
        FMT(10, right) << sim->now() << " |" <<
        FMT(12, right) << "manager: " << FMT(15, left) << name << "|" <<
        FMT(12, right) << "parameter: " << FMT(15, left) << param << "| " <<
        value[index] << std::endl;

      set(value[index]);
      index++;
      if (index == duration.size()) {
        if (period < 0)
          goto end;
        index = 1;
      }

      sim->schedule(duration[index], this, priority);
      end:
        return;
    }

    bool activate(double delay = 0) { return Process::activate(duration[index]); }

  private:
    std::string param;
    VEC<double> duration;
    VEC<int> value;
    int period;
    Setter set;
    size_t index;
  };

} // namespace simmer

#endif
