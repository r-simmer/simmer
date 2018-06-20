// Copyright (C) 2014-2018 Iñaki Ucar and Bart Smeets
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
      if (sim->verbose) sim->print("manager", name, "parameter", param,
          MakeString() << value[index]);

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
