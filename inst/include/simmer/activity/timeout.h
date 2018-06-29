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

#ifndef simmer__activity_timeout_h
#define simmer__activity_timeout_h

#include <simmer/activity.h>

namespace simmer {

  /**
   * Timeout.
   */
  template <typename T>
  class Timeout : public Activity {
  public:
    CLONEABLE(Timeout<T>)

    Timeout(const T& delay) : Activity("Timeout"), delay(delay) {}

    void print(unsigned int indent = 0, bool verbose = false, bool brief = false) {
      Activity::print(indent, verbose, brief);
      internal::print(brief, true, ARG(delay));
    }

    double run(Arrival* arrival) {
      double value = get<double>(delay, arrival);
      if (ISNAN(value))
        Rcpp::stop("missing value (NA or NaN returned)");
      return std::abs(value);
    }

  protected:
    T delay;
  };

} // namespace simmer

#endif
