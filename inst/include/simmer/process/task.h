// Copyright (C) 2016-2018 Iñaki Ucar
//
// This file is part of simmer.
//
// simmer is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// simmer is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with simmer. If not, see <http://www.gnu.org/licenses/>.

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
