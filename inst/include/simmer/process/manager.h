#ifndef simmer__process_manager_h
#define simmer__process_manager_h

#include <simmer/process/process.h>

class Manager : public Process {
  typedef Fn<void(int)> Setter;

public:
  Manager(Simulator* sim, const std::string& name, const std::string& param,
          const VEC<double>& duration, const VEC<int>& value, int period, const Setter& set)
    : Process(sim, name, false, PRIORITY_MANAGER), param(param),
      duration(duration), value(value), period(period), set(set), index(0) {}

  void reset() { index = 0; }
  void run();
  bool activate(double delay = 0) { return Process::activate(duration[index]); }

private:
  std::string param;
  VEC<double> duration;
  VEC<int> value;
  int period;
  Setter set;
  size_t index;
};

#endif
