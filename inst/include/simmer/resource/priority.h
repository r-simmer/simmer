#ifndef simmer__resource_priority_h
#define simmer__resource_priority_h

#include <simmer/resource.h>
#include <simmer/resource/types.h>
#include <simmer/process/arrival.h>

/**
*  Priority resource.
*/
template <typename T>
class PriorityRes : public Resource {
  typedef UMAP<Arrival*, typename T::iterator> ServerMap;

public:
  PriorityRes(Simulator* sim, const std::string& name, int mon, int capacity,
              int queue_size, bool queue_size_strict)
    : Resource(sim, name, mon, capacity, queue_size, queue_size_strict) {}

  ~PriorityRes() { reset(); }

  void reset() {
    Resource::reset();
    foreach_ (RPQueue::value_type& itr, queue)
      delete itr.arrival;
    queue.clear();
    queue_map.clear();
    server.clear();
    server_map.clear();
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
    if (queue_size < 0 || queue_count + amount <= queue_size)
      return true;
    int count = (queue_size > 0) ? (queue_size - queue_count) : 0;
    foreach_r_ (const RPQueue::value_type& itr, queue) {
      if (priority > itr.priority())
        count += itr.amount;
      else
        break;
      if (count >= amount)
        return true;
    }
    return false;
  }

  int try_free_server(bool verbose, double time) { return 0; }

  int try_free_queue(bool verbose, double time) {
    int count = 0;
    RPQueue::iterator last = --queue.end();
    if (verbose) verbose_print(time, last->arrival->name, "REJECT");
    count += last->amount;
    queue_count -= last->amount;
    queue_map.erase(last->arrival);
    last->arrival->unregister_entity(this);
    last->arrival->terminate(false);
    queue.erase(last);
    return count;
  }

  int try_serve_from_queue(bool verbose, double time) {
    int count = 0;
    RPQueue::iterator next = queue.begin();
    if (!room_in_server(next->amount, next->priority()))
      return count;
    next->arrival->restart();
    insert_in_server(verbose, time, next->arrival, next->amount);
    count += next->amount;
    queue_count -= next->amount;
    queue_map.erase(next->arrival);
    queue.erase(next);
    return count;
  }

  void insert_in_server(bool verbose, double time, Arrival* arrival, int amount) {
    if (capacity > 0) while (server_count + amount > capacity)
      try_free_server(verbose, time);
    if (verbose) verbose_print(time, arrival->name, "SERVE");
    server_count += amount;
    typename ServerMap::iterator search = server_map.find(arrival);
    if (search != server_map.end()) {
      search->second->amount += amount;
      arrival->unregister_entity(this);
    } else server_map[arrival] = server.emplace(time, arrival, amount);
  }

  void insert_in_queue(bool verbose, double time, Arrival* arrival, int amount) {
    if (queue_size > 0) while (queue_count + amount > queue_size)
      try_free_queue(verbose, time);
    if (verbose) verbose_print(time, arrival->name, "ENQUEUE");
    queue_count += amount;
    queue_map[arrival] = queue.emplace(time, arrival, amount);
  }

  int remove_from_server(bool verbose, double time, Arrival* arrival, int amount) {
    if (verbose) verbose_print(time, arrival->name, "DEPART");
    typename ServerMap::iterator search = server_map.find(arrival);
    if (search == server_map.end())
      Rcpp::stop("%s: release: not previously seized", name);
    if (search->second->amount < amount)
      Rcpp::stop("%s: release: incorrect amount (%d)", name, amount);
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

  int remove_from_queue(bool verbose, double time, Arrival* arrival) {
    int count = 0;
    QueueMap::iterator search = queue_map.find(arrival);
    if (search == queue_map.end())
      return count;
    if (verbose) verbose_print(time, arrival->name, "DEPART");
    count += search->second->amount;
    queue_count -= search->second->amount;
    search->second->arrival->unregister_entity(this);
    queue.erase(search->second);
    queue_map.erase(search);
    return count;
  }
};

#endif
