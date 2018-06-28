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

#ifndef simmer__resource_types_h
#define simmer__resource_types_h

#include <simmer/process/arrival.h>

namespace simmer {

  struct RSeize {
    double arrived_at;
    Arrival* arrival;
    int amount;

    RSeize(double arrived_at, Arrival* arrival, int amount)
      : arrived_at(arrived_at), arrival(arrival), amount(amount) {}

    int priority() const { return arrival->order.get_priority(); }
    int preemptible() const { return arrival->order.get_preemptible(); }
    bool restart() const { return arrival->order.get_restart(); }
    double remaining() const { return arrival->get_remaining(); }
  };

  struct RQComp {
    bool operator()(const RSeize& lhs, const RSeize& rhs) const {
      if (lhs.priority() == rhs.priority()) {
        if (lhs.arrived_at == rhs.arrived_at)
          return lhs.remaining() > rhs.remaining();
        return lhs.arrived_at < rhs.arrived_at;
      }
      return lhs.priority() > rhs.priority();
    }
  };

  struct RSCompFIFO {
    bool operator()(const RSeize& lhs, const RSeize& rhs) const {
      if (lhs.preemptible() == rhs.preemptible())
        return lhs.arrived_at < rhs.arrived_at;
      return lhs.preemptible() < rhs.preemptible();
    }
  };

  struct RSCompLIFO {
    bool operator()(const RSeize& lhs, const RSeize& rhs) const {
      if (lhs.preemptible() == rhs.preemptible())
        return lhs.arrived_at > rhs.arrived_at;
      return lhs.preemptible() < rhs.preemptible();
    }
  };

  typedef MSET<RSeize, RQComp> RPQueue;
  typedef UMAP<Arrival*, RPQueue::iterator> QueueMap;
  typedef MSET<RSeize, RSCompFIFO> FIFO;
  typedef MSET<RSeize, RSCompLIFO> LIFO;

} // namespace simmer

#endif
