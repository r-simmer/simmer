// Copyright (C) 2018-2024 Iñaki Ucar
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
#include <any>

#define STDANYCAST std::any_cast
#if defined(__clang__) && defined(__apple_build_version__)
#if __apple_build_version__ <= 10001045
namespace simmer { namespace _std {

  template <class T>
  T any_cast(const std::any& operand) {
    if (!operand.has_value() || typeid(T) != operand.type())
      throw std::runtime_error("bad_any_cast");
    return *std::any_cast<T>(&operand);
  }

}}
#undef  STDANYCAST
#define STDANYCAST simmer::_std::any_cast
#endif
#endif

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
      : Process(sim, name_prefix, mon, PRIORITY_MIN), order(order), trj_(trj) {}

    virtual void reset() {
      count = 0;
      trj = trj_;
      head = internal::head(trj);
      ahead.clear();
    }

    virtual bool deactivate() {
      for (auto arrival : ahead) {
        arrival->deactivate();
        delete arrival;
        count--;
      }
      ahead.clear();
      return Process::deactivate();
    }

    int get_count() const { return count; }

    REnv get_trajectory() const { return trj; }

    void set_source(const std::any& new_source) {
      deactivate();
      set_source_impl(new_source);
      activate();
    }

    void set_trajectory(const REnv& new_trj) {
      trj = new_trj;
      head = internal::head(trj);
    }

  protected:
    Arrival* new_arrival(double delay) {
      // format the name and create the next arrival
      std::string arr_name = MakeString() << name << count++;
      Arrival* arrival = new Arrival(
        sim, arr_name, is_monitored(), order, head, count, this);
      ahead.emplace(arrival);

      if (sim->verbose) sim->print("source", name, "new", arr_name,
          MakeString() << (sim->now() + delay));

      // schedule the arrival
      sim->schedule(delay, arrival, head && head->priority ?
                      head->priority : count);

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
    int count;                /**< number of arrivals generated */
    Order order;
    Activity* head;
    REnv trj_, trj;
    ArrSet ahead;

    virtual void set_source_impl(const std::any& new_source) = 0;
  };

} // namespace simmer

#endif
