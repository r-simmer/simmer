// Copyright (C) 2015-2016 Bart Smeets and Iñaki Ucar
// Copyright (C) 2016-2018,2020 Iñaki Ucar
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

#ifndef simmer__activity_branch_h
#define simmer__activity_branch_h

#include <simmer/activity.h>
#include <simmer/activity/fork.h>
#include <simmer/activity/storage.h>

namespace simmer {

  /**
   * Branch. It runs as another activity, but encloses other trajectories
   * that are selected at runtime through a user-defined function.
   */
  class Branch : public Fork {
  public:
    CLONEABLE(Branch)

    Branch(const RFn& option, const VEC<bool>& cont, const VEC<REnv>& trj)
      : Activity("Branch"), Fork(cont, trj), option(option) {}

    void print(unsigned int indent = 0, bool verbose = false, bool brief = false) {
      Activity::print(indent, verbose, brief);
      internal::print(brief, false, ARG(option));
      Fork::print(indent, verbose, brief);
    }

    double run(Arrival* arrival) {
      int ret = get<int>(option, arrival);
      if (ret < 0 || ret > (int)heads.size())
        Rcpp::stop("index out of range");
      if (ret) path = ret-1;
      return 0;
    }

  protected:
    RFn option;
  };

  /**
   * Clone an arrival.
   */
  template <typename T>
  class Clone : public Fork {
  public:
    CLONEABLE(Clone<T>)

    Clone(const T& n, const VEC<REnv>& trj)
      : Activity("Clone"), Fork(VEC<bool>(trj.size(), true), trj), n(n) {}

    void print(unsigned int indent = 0, bool verbose = false, bool brief = false) {
      Activity::print(indent, verbose, brief);
      internal::print(brief, false, ARG(n));
      Fork::print(indent, verbose, brief);
    }

    double run(Arrival* arrival) {
      unsigned int ret = (unsigned int) std::abs(get<int>(n, arrival));
      for (unsigned int i = 1; i < ret; i++) {
        if (i < heads.size())
          path = i;
        Arrival* new_arrival = arrival->clone();
        new_arrival->set_activity(get_next());
        new_arrival->activate();
      }
      if (heads.size())
        path = 0;
      return 0;
    }

  protected:
    T n;
  };

  /**
   * Synchronize clones.
   */
  class Synchronize : public virtual Activity, public Storage<std::string, int> {
  public:
    CLONEABLE(Synchronize)

    Synchronize(bool wait, bool terminate)
      : Activity("Synchronize"), wait(wait), terminate(terminate) {}

    Synchronize(const Synchronize& o)
      : Activity(o), wait(o.wait), terminate(o.terminate) {}

    void print(unsigned int indent = 0, bool verbose = false, bool brief = false) {
      Activity::print(indent, verbose, brief);
      internal::print(brief, true, ARG(wait));
    }

    double run(Arrival* arrival) {
      if (!wait) {
        if (!storage_find(arrival)) {
          if (arrival->get_clones() > 1)
            storage_get(arrival) = arrival->get_clones() - 1;
          return 0;
        } else if (!--storage_get(arrival))
          remove(arrival);
      } else if (arrival->get_clones() == 1)
        return 0;

      if (!terminate)
        delete arrival;
      else
        arrival->terminate(true);
      return STATUS_REJECT;
    }

  protected:
    bool wait;
    bool terminate;
  };

} // namespace simmer

#endif
