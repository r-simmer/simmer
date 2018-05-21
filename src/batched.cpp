#include <simmer/process/batched.h>
#include <simmer/simulator.h>

void Batched::terminate(bool finished) {
  foreach_ (Arrival* arrival, arrivals)
    arrival->terminate(finished);
  arrivals.clear();
  Arrival::terminate(finished);
}

void Batched::set_attribute(const std::string& key, double value, bool global) {
  if (global) return sim->set_attribute(key, value);
  attributes[key] = value;
  foreach_ (Arrival* arrival, arrivals)
    arrival->set_attribute(key, value);
}

void Batched::erase(Arrival* arrival) {
  bool del = activity;
  if (arrivals.size() > 1 || (batch && batch->is_permanent())) {
    del = false;
    if (arrival->is_monitored()) {
      Batched* up = this;
      while (up) {
        up->report(arrival);
        up = up->batch;
      }
    }
  } else if (arrivals.size() == 1 && !batch) {
    if (!leave_resources(!activity))
      deactivate();
  } else {
    del = true;
    batch->erase(this);
    leave_resources();
  }
  arrivals.erase(std::remove(arrivals.begin(), arrivals.end(), arrival), arrivals.end());
  arrival->unregister_entity(this);
  if (del) delete this;
}

void Batched::report(Arrival* arrival) const {
  foreach_ (const ResTime::value_type& itr, restime)
    arrival->report(itr.first, itr.second.start,
                    itr.second.activity - status.busy_until + sim->now());
}
