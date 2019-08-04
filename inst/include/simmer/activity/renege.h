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

#ifndef simmer__activity_renege_h
#define simmer__activity_renege_h

#include <simmer/activity/fork.h>

namespace simmer {

  /**
   * Leave the trajectory with some probability.
   */
  template <typename T>
  class Leave : public Fork {
  public:
    CLONEABLE(Leave<T>)

    Leave(const T& prob, const VEC<REnv>& trj, bool keep_seized)
      : Fork("Leave", VEC<bool>(trj.size(), false), trj), prob(prob),
        keep_seized(keep_seized) {}

    void print(unsigned int indent = 0, bool verbose = false, bool brief = false) {
      Activity::print(indent, verbose, brief);
      internal::print(brief, false, ARG(prob), ARG(keep_seized));
      Fork::print(indent, verbose, brief);
    }

    double run(Arrival* arrival) {
      Activity* next = NULL;
      if (Rcpp::runif(1)[0] > get<double>(prob, arrival))
        return 0;
      if (heads.size())
        next = heads[0];
      arrival->set_renege(0, next, keep_seized);
      return 0;
    }

  protected:
    T prob;
    bool keep_seized;
  };

  /**
   * Renege after some time.
   */
  template <typename T>
  class RenegeIn : public Fork {
  public:
    CLONEABLE(RenegeIn<T>)

    RenegeIn(const T& t, const VEC<REnv>& trj, bool keep_seized)
      : Fork("RenegeIn", VEC<bool>(trj.size(), false), trj), t(t),
        keep_seized(keep_seized) {}

    void print(unsigned int indent = 0, bool verbose = false, bool brief = false) {
      Activity::print(indent, verbose, brief);
      internal::print(brief, false, ARG(t), ARG(keep_seized));
      Fork::print(indent, verbose, brief);
    }

    double run(Arrival* arrival) {
      Activity* next = NULL;
      if (heads.size())
        next = heads[0];
      arrival->set_renege(std::abs(get<double>(t, arrival)), next, keep_seized);
      return 0;
    }

  protected:
    T t;
    bool keep_seized;
  };

  /**
   * Renege if a signal is received.
   */
  template <typename T>
  class RenegeIf : public Fork {
  public:
    CLONEABLE(RenegeIf<T>)

    RenegeIf(const T& signal, const VEC<REnv>& trj, bool keep_seized)
      : Fork("RenegeIf", VEC<bool>(trj.size(), false), trj), signal(signal),
        keep_seized(keep_seized) {}

    void print(unsigned int indent = 0, bool verbose = false, bool brief = false) {
      Activity::print(indent, verbose, brief);
      internal::print(brief, false, ARG(signal), ARG(keep_seized));
      Fork::print(indent, verbose, brief);
    }

    double run(Arrival* arrival) {
      Activity* next = NULL;
      if (heads.size())
        next = heads[0];
      arrival->set_renege(get<std::string>(signal, arrival), next, keep_seized);
      return 0;
    }

  protected:
    T signal;
    bool keep_seized;
  };

  /**
   * Abort reneging.
   */
  class RenegeAbort : public Activity {
  public:
    CLONEABLE(RenegeAbort)

    RenegeAbort() : Activity("RenegeAbort") {}

    void print(unsigned int indent = 0, bool verbose = false, bool brief = false) {
      Activity::print(indent, verbose, brief);
      internal::print(brief, true);
    }

    double run(Arrival* arrival) {
      arrival->cancel_renege();
      return 0;
    }
  };

  /**
   * Set a path to handle unfinished arrivals (from 'leave' or resources)
   */
  class HandleUnfinished : public Fork {
  public:
    CLONEABLE(HandleUnfinished)

    HandleUnfinished(const VEC<REnv>& trj)
      : Fork("HandleUnfinished", VEC<bool>(trj.size(), false), trj) {}

    void print(unsigned int indent = 0, bool verbose = false, bool brief = false) {
      Activity::print(indent, verbose, brief);
      internal::print(brief, false);
      Fork::print(indent, verbose, brief);
    }

    double run(Arrival* arrival) {
      Activity* next = NULL;
      if (heads.size())
        next = heads[0];
      arrival->set_dropout(next);
      return 0;
    }
  };

} // namespace simmer

#endif
