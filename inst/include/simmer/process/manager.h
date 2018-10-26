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

#ifndef simmer__process_manager_h
#define simmer__process_manager_h

#include <simmer/process.h>

namespace simmer {

  template <typename T>
  class Manager : public Process {
    typedef Fn<void(T)> Setter;

  public:
    Manager(Simulator* sim, const std::string& name,
            const VEC<double>& duration, const VEC<T>& value, int period,
            const Setter& set, const OPT<T>& init = NONE)
      : Process(sim, name, false, PRIORITY_MANAGER), duration(duration),
        value(value), period(period), set(set), init(init), index(0) { reset(); }

    void reset() {
      index = 0;
      if (init && (duration.empty() || duration[index]))
        set(*init);
    }

    void run() {
      if (sim->verbose)
        sim->print("manager", name, "value", MakeString() << value[index]);

      set(value[index]);

      if (++index == duration.size()) {
        if (period < 0)
          goto end;
        index = 1;
      }

      sim->schedule(duration[index], this, priority);
    end:
      return;
    }

    bool activate(double delay = 0) {
      if (duration.empty())
        return false;
      return Process::activate(duration[index]);
    }

  private:
    VEC<double> duration;
    VEC<T> value;
    int period;
    Setter set;
    OPT<T> init;
    size_t index;
  };

} // namespace simmer

#endif
