// Copyright (C) 2018,2021 Iñaki Ucar
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

#ifndef simmer__process_source_h
#define simmer__process_source_h

#include <simmer/process.h>
#include <simmer/process/order.h>
#include <simmer/process/arrival.h>
#include <simmer/activity.h>

namespace simmer {
  /**
   * Abstract class for source processes.
   */
  class Source : public Process {
    friend class Arrival;

  public:
    typedef USET<Arrival*> ArrSet;

    /**
    * Constructor.
    * @param sim             a pointer to the simulator
    * @param name_prefix     name prefix for each new arrival
    * @param mon             int that indicates whether this entity must be monitored
    * @param trj             a user-defined R trajectory
    * @param source          some source for arrivals
    * @param order           priority, preemptible, restart
    */
    Source(Simulator* sim, const std::string& name_prefix, int mon,
           const REnv& trj, const Order& order)
      : Process(sim, name_prefix, mon, PRIORITY_MIN), count(0), order(order),
        first_activity(internal::head(trj)), trj(trj) {}

    virtual void reset() { count = 0; }

    virtual bool deactivate() {
      foreach_ (Arrival* arrival, ahead) {
        count--;
        arrival->deactivate();
        delete arrival;
      }
      ahead.clear();
      return Process::deactivate();
    }

    int get_n_generated() const { return count; }

    REnv get_trajectory() const { return trj; }

    virtual void set_source(const ANY& new_source) = 0;

    void set_trajectory(const REnv& new_trj) {
      trj = new_trj;
      first_activity = internal::head(trj);
    }

  protected:
    int count;                /**< number of arrivals generated */
    Order order;
    Activity* first_activity;

    Arrival* new_arrival(double delay) {
      // format the name and create the next arrival
      std::string arr_name = MakeString() << name << count++;
      Arrival* arrival = new Arrival(
        sim, arr_name, is_monitored(), order, first_activity, count, this);
      ahead.emplace(arrival);

      if (sim->verbose) sim->print("source", name, "new", arr_name,
          MakeString() << (sim->now() + delay));

      // schedule the arrival
      sim->schedule(delay, arrival, first_activity && first_activity->priority ?
                      first_activity->priority : count);

      return arrival;
    }

    void unregister_arrival(Arrival* arrival) {
      ahead.erase(arrival);
    }

    bool check_stop(double delay) {
      if (delay < 0 || RNum::is_na(delay))
        return true;
      return false;
    }

  private:
    REnv trj;
    ArrSet ahead;
  };

} // namespace simmer

#endif
