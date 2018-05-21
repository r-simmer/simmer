#ifndef simmer__resource_preemptive_h
#define simmer__resource_preemptive_h

#include <simmer/process.h>
#include <simmer/resource/resource.h>
#include <simmer/resource/priority.h>
#include <simmer/resource/types.h>

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
  using Resource::capacity;
  using Resource::server_count;
  using Resource::queue_count;
  using Resource::queue_size_strict;
  using Resource::verbose_print;

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

  int try_free_server(bool verbose, double time) {
    int count = 0;
    typename T::iterator first = server.begin();
    if (first == server.end())
      return count;
    first->arrival->pause();
    if (verbose) verbose_print(time, first->arrival->name, "PREEMPT");
    count += first->amount;
    server_count -= first->amount;
    server_map.erase(first->arrival);
    if (queue_size_strict) {
      if (!room_in_queue(first->amount, first->priority())) {
        if (verbose) verbose_print(time, first->arrival->name, "REJECT");
        first->arrival->unregister_entity(this);
        first->arrival->terminate(false);
      } else insert_in_queue(verbose, time, first->arrival, first->amount);
    } else {
      preempted_map[first->arrival] = preempted.insert(*first);
      queue_count += first->amount;
    }
    server.erase(first);
    return count;
  }

  int try_serve_from_queue(bool verbose, double time) {
    int count = 0;
    RPQueue::iterator next = preempted.begin();
    if (next == preempted.end())
      return PriorityRes<T>::try_serve_from_queue(verbose, time);
    if (!room_in_server(next->amount, next->priority()))
      return count;
    next->arrival->restart();
    insert_in_server(verbose, time, next->arrival, next->amount);
    count += next->amount;
    queue_count -= next->amount;
    preempted_map.erase(next->arrival);
    preempted.erase(next);
    return count;
  }

  int remove_from_queue(bool verbose, double time, Arrival* arrival) {
    int count = PriorityRes<T>::remove_from_queue(verbose, time, arrival);
    QueueMap::iterator search = preempted_map.find(arrival);
    if (count || search == preempted_map.end())
      return count;
    if (verbose) verbose_print(time, arrival->name, "DEPART");
    count += search->second->amount;
    queue_count -= search->second->amount;
    search->second->arrival->unregister_entity(this);
    preempted.erase(search->second);
    preempted_map.erase(search);
    return count;
  }
};

#endif
