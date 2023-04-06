// Copyright (C) 2015-2016 Bart Smeets and Iñaki Ucar
// Copyright (C) 2016-2023 Iñaki Ucar
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

#ifndef simmer__resource_priority_h
#define simmer__resource_priority_h

#include <simmer/resource.h>
#include <simmer/resource/types.h>
#include <simmer/process/arrival.h>

namespace simmer {

  /**
  *  Priority resource.
  */
  template <typename T>
  class PriorityRes : public Resource {
    typedef UMAP<Arrival*, typename T::iterator> ServerMap;

  public:
    PriorityRes(Simulator* sim, const std::string& name, int mon, int capacity,
                int queue_size, bool queue_size_strict, int queue_priority_min,
                int queue_priority_max)
      : Resource(sim, name, mon, capacity, queue_size, queue_size_strict,
                 queue_priority_min, queue_priority_max) {}

    ~PriorityRes() { reset(); }

    void reset() {
      Resource::reset();
      for (auto& itr : queue)
        delete itr.arrival;
      queue.clear();
      queue_map.clear();
      server.clear();
      server_map.clear();
    }

    int get_seized(Arrival* arrival) const {
      typename ServerMap::const_iterator search = server_map.find(arrival);
      if (search != server_map.end())
        return search->second->amount;
      return 0;
    }

    bool is_waiting(Arrival* arrival) const {
      typename QueueMap::const_iterator search = queue_map.find(arrival);
      if (search != queue_map.end())
        return true;
      return false;
    }

  protected:
    T server;
    ServerMap server_map;
    RPQueue queue;
    QueueMap queue_map;

    bool first_in_line(int priority) const {
      if (queue.empty() || queue.begin()->priority() < priority)
        return true;
      return false;
    }

    bool room_in_server(int amount, int priority) const {
      return capacity < 0 || server_count + amount <= capacity;
    }

    bool room_in_queue(int amount, int priority) const {
      if (queue_priority_min < 0 || priority < queue_priority_min)
        return false;
      if (queue_priority_max >= 0 && priority > queue_priority_max)
        return false;
      if (queue_size < 0 || queue_count + amount <= queue_size)
        return true;
      int count = (queue_size > 0) ? (queue_size - queue_count) : 0;
      for (auto itr = queue.rbegin(); itr != queue.rend(); ++itr) {
        if (priority > itr->priority())
          count += itr->amount;
        else
          break;
        if (count >= amount)
          return true;
      }
      return false;
    }

    int try_free_server() { return 0; }

    int try_free_queue() {
      int count = 0;
      RPQueue::iterator last = --queue.end();
      if (sim->verbose) print(last->arrival->name, "REJECT");
      count += last->amount;
      queue_count -= last->amount;
      queue_map.erase(last->arrival);
      last->arrival->restart();
      last->arrival->stop();
      last->arrival->unregister_entity(this);
      last->arrival->terminate(false);
      queue.erase(last);
      return count;
    }

    int try_serve_from_queue() {
      int count = 0;
      RPQueue::iterator next = queue.begin();
      if (!room_in_server(next->amount, next->priority()))
        return count;
      next->arrival->restart();
      insert_in_server(next->arrival, next->amount);
      count += next->amount;
      queue_count -= next->amount;
      queue_map.erase(next->arrival);
      queue.erase(next);
      return count;
    }

    void insert_in_server(Arrival* arrival, int amount) {
      if (capacity > 0) while (server_count + amount > capacity)
        try_free_server();
      if (sim->verbose) print(arrival->name, "SERVE");
      server_count += amount;
      typename ServerMap::iterator search = server_map.find(arrival);
      if (search != server_map.end()) {
        search->second->amount += amount;
        arrival->unregister_entity(this);
      } else server_map[arrival] = server.emplace(sim->now(), arrival, amount);
    }

    void insert_in_queue(Arrival* arrival, int amount) {
      if (queue_size > 0) while (queue_count + amount > queue_size)
        try_free_queue();
      if (sim->verbose) print(arrival->name, "ENQUEUE");
      queue_count += amount;
      queue_map[arrival] = queue.emplace(sim->now(), arrival, amount);
    }

    int remove_from_server(Arrival* arrival, int amount) {
      if (sim->verbose) print(arrival->name, "DEPART");
      typename ServerMap::iterator search = server_map.find(arrival);
      if (search == server_map.end())
        Rcpp::stop("'%s' not previously seized", name);
      if (search->second->amount < amount)
        Rcpp::stop("incorrect amount for '%s' (%d)", name, amount);
      else if (amount < 0 || amount == search->second->amount) {
        server_count -= search->second->amount;
        amount = search->second->amount;
        server.erase(search->second);
        server_map.erase(search);
      } else {
        server_count -= amount;
        search->second->amount -= amount;
        arrival->register_entity(this);
      }
      return amount;
    }

    int remove_from_queue(Arrival* arrival) {
      int count = 0;
      QueueMap::iterator search = queue_map.find(arrival);
      if (search == queue_map.end())
        return count;
      if (sim->verbose) print(arrival->name, "DEPART");
      count += search->second->amount;
      queue_count -= search->second->amount;
      search->second->arrival->unregister_entity(this);
      queue.erase(search->second);
      queue_map.erase(search);
      return count;
    }
  };

} // namespace simmer

#endif
