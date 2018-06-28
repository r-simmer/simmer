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

#ifndef simmer__activity_leave_h
#define simmer__activity_leave_h

#include <simmer/activity.h>

namespace simmer {

  /**
   * Leave the trajectory with some probability.
   */
  template <typename T>
  class Leave : public Activity {
  public:
    CLONEABLE(Leave<T>)

    Leave(const T& prob) : Activity("Leave"), prob(prob) {}

    void print(unsigned int indent = 0, bool verbose = false, bool brief = false) {
      Activity::print(indent, verbose, brief);
      internal::print(brief, true, ARG(prob));
    }

    double run(Arrival* arrival) {
      if (Rcpp::runif(1)[0] > get<double>(prob, arrival))
        return 0;
      arrival->terminate(false);
      return REJECT;
    }

  protected:
    T prob;
  };

} // namespace simmer

#endif
