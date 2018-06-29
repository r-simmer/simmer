// Copyright (C) 2015-2016 Bart Smeets and Iñaki Ucar
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

#ifndef simmer__process_h
#define simmer__process_h

#include <simmer/entity.h>

namespace simmer {

  /**
   * Abstract class for processes, active entities that need a method run().
   */
  class Process : public Entity {
  public:
    Process(Simulator* sim, const std::string& name, int mon, int priority = 0)
      : Entity(sim, name, mon), priority(priority) {}

    virtual void run() = 0;

    virtual bool activate(double delay = 0) {
      sim->schedule(delay, this, priority);
      return true;
    }

    virtual bool deactivate() {
      if (!sim->is_scheduled(this))
        return false;
      sim->unschedule(this);
      return true;
    }

  protected:
    int priority;
  };

} // namespace simmer

#endif
