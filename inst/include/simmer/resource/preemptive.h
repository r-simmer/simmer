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

#ifndef simmer__resource_preemptive_h
#define simmer__resource_preemptive_h

#include <simmer/resource.h>
#include <simmer/resource/priority.h>
#include <simmer/resource/types.h>
#include <simmer/process/arrival.h>

namespace simmer {

  /**
  *  Preemptive resource.
  */
  template <typename T>
  class PreemptiveRes : public PriorityRes<T> {
  public:
    PreemptiveRes(Simulator* sim, const std::string& name, int mon, int capacity,
                  int queue_size, bool queue_size_strict)
      : PriorityRes<T>(sim, name, mon, capacity, queue_size, queue_size_strict) {}

    ~PreemptiveRes() { reset(); }

    void reset() {
      PriorityRes<T>::reset();
      foreach_ (RPQueue::value_type& itr, preempted)
        delete itr.arrival;
      preempted.clear();
      preempted_map.clear();
    }

  protected:
    using Resource::sim;
    using Resource::capacity;
    using Resource::server_count;
    using Resource::queue_count;
    using Resource::queue_size_strict;
    using Resource::print;

    using PriorityRes<T>::server;
    using PriorityRes<T>::server_map;
    using PriorityRes<T>::room_in_queue;
    using PriorityRes<T>::insert_in_server;
    using PriorityRes<T>::insert_in_queue;

    RPQueue preempted;
    QueueMap preempted_map;

    bool first_in_line(int priority) const {
      if (!preempted.empty() && preempted.begin()->priority() >= priority)
        return false;
      return PriorityRes<T>::first_in_line(priority);
    }

    bool room_in_server(int amount, int priority) const {
      if (PriorityRes<T>::room_in_server(amount, priority))
        return true;
      int count = (capacity > 0) ? (capacity - server_count) : 0;
      foreach_ (const typename T::value_type& itr, server) {
        if (priority > itr.preemptible())
          count += itr.amount;
        else
          break;
        if (count >= amount)
          return true;
      }
      return false;
    }

    int try_free_server() {
      int count = 0;
      typename T::iterator first = server.begin();
      if (first == server.end())
        return count;
      first->arrival->pause();
      if (sim->verbose) print(first->arrival->name, "PREEMPT");
      count += first->amount;
      server_count -= first->amount;
      server_map.erase(first->arrival);
      if (queue_size_strict) {
        if (!room_in_queue(first->amount, first->priority())) {
          if (sim->verbose) print(first->arrival->name, "REJECT");
          first->arrival->unregister_entity(this);
          first->arrival->terminate(false);
        } else insert_in_queue(first->arrival, first->amount);
      } else {
        preempted_map[first->arrival] = preempted.insert(*first);
        queue_count += first->amount;
      }
      server.erase(first);
      return count;
    }

    int try_serve_from_queue() {
      int count = 0;
      RPQueue::iterator next = preempted.begin();
      if (next == preempted.end())
        return PriorityRes<T>::try_serve_from_queue();
      if (!room_in_server(next->amount, next->priority()))
        return count;
      next->arrival->restart();
      insert_in_server(next->arrival, next->amount);
      count += next->amount;
      queue_count -= next->amount;
      preempted_map.erase(next->arrival);
      preempted.erase(next);
      return count;
    }

    int remove_from_queue(Arrival* arrival) {
      int count = PriorityRes<T>::remove_from_queue(arrival);
      QueueMap::iterator search = preempted_map.find(arrival);
      if (count || search == preempted_map.end())
        return count;
      if (sim->verbose) print(arrival->name, "DEPART");
      count += search->second->amount;
      queue_count -= search->second->amount;
      search->second->arrival->unregister_entity(this);
      preempted.erase(search->second);
      preempted_map.erase(search);
      return count;
    }
  };

} // namespace simmer

#endif
