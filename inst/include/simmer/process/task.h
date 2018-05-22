#ifndef simmer__process_task_h
#define simmer__process_task_h

#include <simmer/process.h>

namespace simmer {

  class Task : public Process {
    typedef Fn<void()> Callback;

  public:
    Task(Simulator* sim, const std::string& name, const Callback& task, int priority = 0)
      : Process(sim, name, false, priority), task(task) {}
    ~Task() { reset(); }

    void reset() {}
    void run() {
      if (sim->verbose) Rcpp::Rcout <<
        FMT(10, right) << sim->now() << " |" <<
        FMT(12, right) << "task: " << FMT(15, left) << name << "|" <<
        FMT(12+16, right) << "|" << std::endl;

      task();
      delete this;
    }

  private:
    Callback task;
  };

} // namespace simmer

#endif
