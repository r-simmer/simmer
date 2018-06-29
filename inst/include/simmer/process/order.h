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

#ifndef simmer__process_order_h
#define simmer__process_order_h

#include <simmer/common.h>

namespace simmer {

  struct Order {
  public:
    Order(int priority=0, int preemptible=0, bool restart=false)
      : preemptible(preemptible)
    {
      set_priority(priority);
      set_preemptible(preemptible);
      set_restart(restart);
    }

    void set_priority(int value) {
      priority = value;
      if (preemptible < priority)
        preemptible = priority;
    }
    int get_priority() const { return priority; }
    void set_preemptible(int value) {
      if (value < priority) {
        Rcpp::warning("`preemptible` level cannot be < `priority`, `preemptible` set to %d", priority);
        value = priority;
      }
      preemptible = value;
    }
    int get_preemptible() const { return preemptible; }
    void set_restart(bool value) { restart = value; }
    bool get_restart() const { return restart; }

  private:
    int priority;       /**< arrival priority */
    int preemptible;    /**< maximum priority that cannot cause preemption (>=priority) */
    bool restart;       /**< whether activity must be restarted after preemption */
  };

} // namespace simmer

#endif
