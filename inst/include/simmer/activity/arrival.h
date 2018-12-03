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

#ifndef simmer__activity_arrival_h
#define simmer__activity_arrival_h

#include <simmer/activity.h>
#include <simmer/process/batched.h>

namespace simmer {

  /**
   * Set attributes.
   */
  template <typename T, typename U>
  class SetAttribute : public Activity {
  public:
    CLONEABLE(SetAttribute<T COMMA U>)

    SetAttribute(const T& keys, const U& values, bool global, char mod='N', double init=0)
      : Activity("SetAttribute"), keys(keys), values(values), global(global),
        mod(mod), op(internal::get_op<double>(mod)), init(init) {}

    void print(unsigned int indent = 0, bool verbose = false, bool brief = false) {
      Activity::print(indent, verbose, brief);
      internal::print(brief, true, ARG(keys), ARG(values), ARG(global), ARG(mod), ARG(init));
    }

    double run(Arrival* arrival) {
      VEC<std::string> ks = get<VEC<std::string> >(keys, arrival);
      VEC<double> vals = get<VEC<double> >(values, arrival);

      if (ks.size() != vals.size())
        Rcpp::stop("number of keys and values don't match");

      if (op) for (unsigned int i = 0; i < ks.size(); i++) {
        double attr = arrival->get_attribute(ks[i], global);
        if (ISNA(attr)) attr = init;
        arrival->set_attribute(ks[i], op(attr, vals[i]), global);
      } else for (unsigned int i = 0; i < ks.size(); i++)
        arrival->set_attribute(ks[i], vals[i], global);

      return 0;
    }

  protected:
    T keys;
    U values;
    bool global;
    char mod;
    Fn<double(double, double)> op;
    double init;
  };

  /**
   * Set prioritization.
   */
  template <typename T>
  class SetPrior : public Activity {
  public:
    CLONEABLE(SetPrior<T>)

    SetPrior(const T& values, char mod='N')
      : Activity("SetPrior"), values(values), mod(mod), op(internal::get_op<int>(mod)) {}

    void print(unsigned int indent = 0, bool verbose = false, bool brief = false) {
      Activity::print(indent, verbose, brief);
      internal::print(brief, true, ARG(values), ARG(mod));
    }

    double run(Arrival* arrival) {
      VEC<int> ret = get<VEC<int> >(values, arrival);
      if (ret.size() != 3)
        Rcpp::stop("3 values needed, %u received", ret.size());

      if (op) {
        ret[0] = op(arrival->order.get_priority(), ret[0]);
        ret[1] = op(arrival->order.get_preemptible(), ret[1]);
        ret[2] = op((int)arrival->order.get_restart(), ret[2]);
      }
      if (ret[0] >= 0) arrival->order.set_priority(ret[0]);
      if (ret[1] >= 0) arrival->order.set_preemptible(ret[1]);
      if (ret[2] >= 0) arrival->order.set_restart((bool)ret[2]);

      return 0;
    }

  protected:
    T values;
    char mod;
    Fn<int(int, int)> op;
  };

  /**
   * Create a batch.
   */
  template <typename T>
  class Batch : public Activity {
  public:
    CLONEABLE(Batch<T>)

    Batch(int n, const T& timeout, bool permanent,
          const std::string& id = "", const OPT<RFn>& rule = NONE)
      : Activity("Batch"),
        n(n), timeout(timeout), permanent(permanent), id(id), rule(rule) {}

    void print(unsigned int indent = 0, bool verbose = false, bool brief = false) {
      Activity::print(indent, verbose, brief);
      internal::print(brief, true, ARG(n), ARG(timeout), ARG(permanent), ARG(id));
    }

    double run(Arrival* arrival) {
      if (rule && !get<bool>(*rule, arrival))
        return 0;
      Batched** ptr = arrival->sim->get_batch(this, id);
      if (!(*ptr))
        *ptr = init(arrival);
      (*ptr)->insert(arrival);
      if ((int)(*ptr)->size() == n)
        trigger(arrival->sim, *ptr);
      return STATUS_REJECT;
    }

  protected:
    int n;
    T timeout;
    bool permanent;
    std::string id;
    OPT<RFn> rule;

    Batched* init(Arrival* arrival) {
      std::string str;
      Batched* ptr = NULL;
      if (id.size()) {
        str = "batch_" + id;
        ptr = new Batched(arrival->sim, str, permanent);
      } else {
        int count = arrival->sim->get_batch_count();
        str = MakeString() << "batch" << count;
        ptr = new Batched(arrival->sim, str, permanent, count);
      }
      double dt = std::abs(get<double>(timeout, arrival));
      if (dt) {
        Task* task = new Task(arrival->sim, "Batch-Timer",
                              BIND(&Batch::trigger, this, arrival->sim, ptr),
                              PRIORITY_MIN);
        task->activate(dt);
      }
      return ptr;
    }

    void trigger(Simulator* sim, Batched* target) {
      Batched** ptr = sim->get_batch(this, id);
      if (!(*ptr) || *ptr != target)
        return;
      if ((*ptr)->size()) {
        (*ptr)->set_activity(get_next());
        (*ptr)->activate();
        *ptr = init(*ptr);
      } else {
        delete *ptr;
        *ptr = NULL;
      }
    }
  };

  /**
   * Separate a batch.
   */
  class Separate : public Activity {
  public:
    CLONEABLE(Separate)

    Separate() : Activity("Separate") {}

    void print(unsigned int indent = 0, bool verbose = false, bool brief = false) {
      Activity::print(indent, verbose, brief);
      internal::print(brief, true);
    }

    double run(Arrival* arrival) {
      Batched* batched = dynamic_cast<Batched*>(arrival);
      if (!batched || !batched->pop_all(get_next()))
        return 0;
      return STATUS_REJECT;
    }
  };

} // namespace simmer

#endif
