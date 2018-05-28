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
      if (sim->verbose) sim->print("task", name);
      task();
      delete this;
    }

  private:
    Callback task;
  };

} // namespace simmer

#endif
