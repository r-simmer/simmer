#ifndef TASK_H
#define TASK_H

#include "process.h"

class Task : public Process {
  typedef Fn<void()> Callback;

public:
  Task(Simulator* sim, const std::string& name, const Callback& task, int priority = 0)
    : Process(sim, name, false, priority), task(task) {}
  ~Task() { reset(); }

  void reset() {}
  void run();

private:
  Callback task;
};

#endif
