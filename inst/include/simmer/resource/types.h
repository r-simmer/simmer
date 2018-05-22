#ifndef simmer__resource_types_h
#define simmer__resource_types_h

#include <simmer/process/arrival.h>

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

#endif
