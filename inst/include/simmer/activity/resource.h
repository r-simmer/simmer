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

#ifndef simmer__activity_resource_h
#define simmer__activity_resource_h

#include <simmer/activity.h>
#include <simmer/activity/fork.h>
#include <simmer/activity/resource_internal.h>

namespace simmer {

  /**
   * Seize a resource.
   */
  template <typename T>
  class Seize : public Fork, public internal::ResGetter {
  public:
    CLONEABLE(Seize<T>)

    Seize(const std::string& resource, const T& amount, const VEC<bool>& cont,
          const VEC<REnv>& trj, unsigned short mask)
      : Fork("Seize", cont, trj),
        internal::ResGetter("Seize", resource), amount(amount), mask(mask) {}

    Seize(int id, const T& amount, const VEC<bool>& cont,
          const VEC<REnv>& trj, unsigned short mask)
      : Fork("Seize", cont, trj),
        internal::ResGetter("Seize", id), amount(amount), mask(mask) {}

    void print(unsigned int indent = 0, bool verbose = false, bool brief = false) {
      Activity::print(indent, verbose, brief);
      internal::print(brief, false, ARG(resource), ARG(amount));
      Fork::print(indent, verbose, brief);
    }

    double run(Arrival* arrival) {
      return select_path(arrival, get_resource(arrival)->
                         seize(arrival, std::abs(get<int>(amount, arrival))));
    }

  protected:
    T amount;
    unsigned short mask;

    int select_path(Arrival* arrival, int ret) {
      switch (ret) {
      case REJECT:
        if (mask & 2) {
          ret = SUCCESS;
          if (mask & 1)
            path = 1;
          else
            path = 0;
        } else arrival->terminate(false);
        break;
      default:
        if (mask & 1)
          path = 0;
        break;
      }
      return ret;
    }
  };

  /**
   * Release a resource.
   */
  template <typename T>
  class Release : public Activity, public internal::ResGetter {
  public:
    CLONEABLE(Release<T>)

    Release()
      : Activity("Release", PRIORITY_RELEASE),
        internal::ResGetter("Release"), amount(NONE) {}

    Release(const std::string& resource, const OPT<T>& amount = NONE)
      : Activity("Release", PRIORITY_RELEASE),
        internal::ResGetter("Release", resource), amount(amount) {}

    Release(int id, const OPT<T>& amount = NONE)
      : Activity("Release", PRIORITY_RELEASE),
        internal::ResGetter("Release", id), amount(amount) {}

    void print(unsigned int indent = 0, bool verbose = false, bool brief = false) {
      Activity::print(indent, verbose, brief);
      if (amount)
        internal::print(brief, true, ARG(resource), "amount: ", *amount);
      else internal::print(brief, true, ARG(resource), "amount: ", "all");
    }

    double run(Arrival* arrival) {
      Resource* res = get_resource(arrival);
      if (res) {
        if (*amount)
          return res->release(arrival, std::abs(get<int>(*amount, arrival)));
        return res->release(arrival, res->get_seized(arrival));
      }

      foreach_ (const std::string& resource, arrival->sim->get_resources()) {
        res = arrival->sim->get_resource(resource);
        res->release(arrival, res->get_seized(arrival));
      }
      return 0;
    }

  protected:
    OPT<T> amount;
  };

  /**
   * Set a resource's capacity.
   */
  template <typename T>
  class SetCapacity : public Activity, public internal::ResGetter {
  public:
    CLONEABLE(SetCapacity<T>)

    SetCapacity(const std::string& resource, const T& value, char mod='N')
      : Activity("SetCapacity"), internal::ResGetter("SetCapacity", resource),
        value(value), mod(mod), op(internal::get_op<double>(mod)) {}

    SetCapacity(int id, const T& value, char mod='N')
      : Activity("SetCapacity"), internal::ResGetter("SetCapacity", id),
        value(value), mod(mod), op(internal::get_op<double>(mod)) {}

    void print(unsigned int indent = 0, bool verbose = false, bool brief = false) {
      Activity::print(indent, verbose, brief);
      internal::print(brief, true, ARG(resource), ARG(value), ARG(mod));
    }

    double run(Arrival* arrival) {
      double ret = get<double>(value, arrival);
      double oldval = get_resource(arrival)->get_capacity();
      if (oldval < 0) oldval = R_PosInf;

      if (op) ret = op(oldval, ret);
      if (ret >= 0)
        get_resource(arrival)->set_capacity((ret == R_PosInf) ? -1 : (int) ret);

      if (arrival->is_paused())
        return ENQUEUE;
      return 0;
    }

  protected:
    T value;
    char mod;
    Fn<double(double, double)> op;
  };

  /**
   * Set a resource's queue size.
   */
  template <typename T>
  class SetQueue : public Activity, public internal::ResGetter {
  public:
    CLONEABLE(SetQueue<T>)

    SetQueue(const std::string& resource, const T& value, char mod='N')
      : Activity("SetQueue"), internal::ResGetter("SetQueue", resource),
        value(value), mod(mod), op(internal::get_op<double>(mod)) {}

    SetQueue(int id, const T& value, char mod='N')
      : Activity("SetQueue"), internal::ResGetter("SetQueue", id),
        value(value), mod(mod), op(internal::get_op<double>(mod)) {}

    void print(unsigned int indent = 0, bool verbose = false, bool brief = false) {
      Activity::print(indent, verbose, brief);
      internal::print(brief, true, ARG(resource), ARG(value), ARG(mod));
    }

    double run(Arrival* arrival) {
      double ret = get<double>(value, arrival);
      double oldval = get_resource(arrival)->get_queue_size();
      if (oldval < 0) oldval = R_PosInf;

      if (op) ret = op(oldval, ret);
      if (ret >= 0)
        get_resource(arrival)->set_queue_size((ret == R_PosInf) ? -1 : (int) ret);

      return 0;
    }

  protected:
    T value;
    char mod;
    Fn<double(double, double)> op;
  };

  /**
   * Select a resource based on some policy.
   */
  template <typename T>
  class Select : public Activity {
  public:
    CLONEABLE(Select<T>)

    Select(const T& resources, const std::string& policy, int id)
      : Activity("Select"), resources(resources), id(id), policy(internal::Policy(policy)) {}

    void print(unsigned int indent = 0, bool verbose = false, bool brief = false) {
      Activity::print(indent, verbose, brief);
      internal::print(brief, true, ARG(resources), ARG(policy));
    }

    double run(Arrival* arrival) {
      arrival->set_resource_selected(
          id, policy.dispatch(arrival->sim, get<VEC<std::string> >(resources, arrival)));
      return 0;
    }

  protected:
    T resources;
    int id;
    internal::Policy policy;
  };

} // namespace simmer

#endif
