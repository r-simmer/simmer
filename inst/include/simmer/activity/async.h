// Copyright (C) 2016-2019 Iñaki Ucar
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

#ifndef simmer__activity_async_h
#define simmer__activity_async_h

#include <simmer/activity.h>
#include <simmer/activity/fork.h>

namespace simmer {

  /**
   * Send signals.
   */
  template <typename T, typename U>
  class Send : public Activity {
  public:
    CLONEABLE(Send<T COMMA U>)

    Send(const T& signals, const U& delay)
      : Activity("Send", PRIORITY_SEND), signals(signals), delay(delay) {}

    void print(unsigned int indent = 0, bool verbose = false, bool brief = false) {
      Activity::print(indent, verbose, brief);
      internal::print(brief, true, ARG(signals), ARG(delay));
    }

    double run(Arrival* arrival) {
      double lag = std::abs(get<double>(delay, arrival));
      Task* task =
        new Task(arrival->sim, "Broadcast",
                 BIND(&Simulator::broadcast, arrival->sim,
                      get<VEC<std::string> >(signals, arrival)),
                 lag ? PRIORITY_MIN : PRIORITY_SIGNAL);
      task->activate(lag);
      return 0;
    }

  protected:
    T signals;
    U delay;
  };

  /**
   * Subscribe to signals and assign a handler.
   */
  template <typename T>
  class Trap : public Fork {
  public:
    CLONEABLE(Trap<T>)

    Trap(const T& signals, const VEC<REnv>& trj, bool interruptible)
      : Fork("Trap", VEC<bool>(trj.size(), false), trj, PRIORITY_TRAP),
        signals(signals), interruptible(interruptible)
    {
      if (tails.size() && tails[0])
        tails[0]->set_next(this);
    }

    Trap(const Trap& o) : Fork(o), signals(o.signals), interruptible(o.interruptible) {
      if (tails.size() && tails[0])
        tails[0]->set_next(this);
      pending.clear();
    }

    void print(unsigned int indent = 0, bool verbose = false, bool brief = false) {
      Activity::print(indent, verbose, brief);
      internal::print(brief, false, ARG(signals));
      Fork::print(indent, verbose, brief);
    }

    double run(Arrival* arrival) {
      if (pending.find(arrival) != pending.end()) {
        arrival->set_activity(pending[arrival].back());
        pending[arrival].pop_back();
        if (pending[arrival].empty())
          pending.erase(arrival);
        arrival->activate();
        return STATUS_REJECT;
      }
      arrival->sim->subscribe(get<VEC<std::string> >(signals, arrival), arrival,
                              BIND(&Trap::launch_handler, this, arrival));
      return 0;
    }

  protected:
    T signals;
    bool interruptible;
    UMAP<Arrival*, VEC<Activity*> > pending;

    void launch_handler(Arrival* arrival) {
      if (!arrival->sim->is_scheduled(arrival))
        return;
      arrival->stop();
      if (heads.size() && heads[0]) {
        pending[arrival].push_back(arrival->get_activity());
        arrival->set_activity(heads[0]);
      }
      if (interruptible || heads.empty())
        arrival->sim->subscribe(arrival);
      arrival->activate();
    }
  };

  /**
   * Unsubscribe to signals.
   */
  template <typename T>
  class UnTrap : public Activity {
  public:
    CLONEABLE(UnTrap<T>)

    UnTrap(const T& signals) : Activity("UnTrap", PRIORITY_TRAP), signals(signals) {}

    void print(unsigned int indent = 0, bool verbose = false, bool brief = false) {
      Activity::print(indent, verbose, brief);
      internal::print(brief, true, ARG(signals));
    }

    double run(Arrival* arrival) {
      arrival->sim->unsubscribe(get<VEC<std::string> >(signals, arrival), arrival);
      return 0;
    }

  protected:
    T signals;
  };

  /**
   * Block until a signal is received.
   */
  class Wait : public Activity {
  public:
    CLONEABLE(Wait)

    Wait() : Activity("Wait") {}

    void print(unsigned int indent = 0, bool verbose = false, bool brief = false) {
      Activity::print(indent, verbose, brief);
      internal::print(brief, true);
    }

    double run(Arrival* arrival) { return STATUS_BLOCK; }
  };

} // namespace simmer

#endif
