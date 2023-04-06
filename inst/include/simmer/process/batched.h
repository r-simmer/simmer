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

#ifndef simmer__process_batched_h
#define simmer__process_batched_h

#include <simmer/process/arrival.h>

namespace simmer {

  /**
   *  Batch of arrivals.
   */
  class Batched : public Arrival {
  public:
    CLONEABLE(Batched)
    Task* timer;

    Batched(Simulator* sim, const std::string& name, int n, bool permanent, int priority = 0)
      : Arrival(sim, name, true, Order(), NULL, priority), timer(NULL), n(n), permanent(permanent) {}

    Batched(const Batched& o) : Arrival(o), timer(NULL), arrivals(o.arrivals), permanent(o.permanent) {
      for (size_t i=0; i<arrivals.size(); i++) {
        arrivals[i] = arrivals[i]->clone();
        arrivals[i]->register_entity(this);
      }
    }

    ~Batched() { reset(); }

    void terminate(bool finished) {
      for (auto arrival : arrivals)
        arrival->terminate(finished);
      arrivals.clear();
      Arrival::terminate(finished);
    }

    bool pop_all(Activity* next) {
      if (permanent) return false;
      for (auto arrival : arrivals) {
        arrival->set_activity(next);
        arrival->unregister_entity(this);
        arrival->activate();
      }
      arrivals.clear();
      delete this;
      return true;
    }

    void set_attribute(const std::string& key, double value, bool global=false) {
      if (global) return sim->set_attribute(key, value);
      attributes[key] = value;
      for (auto arrival : arrivals)
        arrival->set_attribute(key, value);
    }

    size_t size() const { return arrivals.size(); }
    int max_size() const { return n; }

    void insert(Arrival* arrival) {
      if ((int)arrivals.size() == n)
        Rcpp::stop("cannot insert into '%s', max. capacity %d reached", name, n); // # nocov
      arrival->set_activity(NULL);
      arrivals.push_back(arrival);
      arrival->register_entity(this);
    }

    bool remove(Arrival* arrival) {
      if (permanent) return false;
      bool del = activity;
      if (arrivals.size() > 1 || (batch && batch->permanent)) {
        del = false;
        if (arrival->is_monitored()) {
          Batched* up = this;
          while (up) {
            up->report(arrival);
            up = up->batch;
          }
        }
      } else if (arrivals.size() == 1 && !batch) {
        leave_resources(false);
        deactivate();
      } else {
        del = true;
        batch->remove(this);
        leave_resources(false);
      }
      arrivals.erase(std::remove(arrivals.begin(), arrivals.end(), arrival), arrivals.end());
      arrival->unregister_entity(this);
      if (del) delete this;
      return true;
    }

  private:
    VEC<Arrival*> arrivals;
    int n;
    bool permanent;

    void reset() {
      for (auto arrival : arrivals)
        delete arrival;
      arrivals.clear();
    }

    void report(const std::string& resource) const {
      for (const auto arrival : arrivals) {
        if (arrival->is_monitored()) {
          ArrTime time = restime.find(resource)->second;
          arrival->report(resource, time.start, time.activity);
        }
      }
    }

    void report(const std::string& resource, double start, double activity) const {
      for (const auto arrival : arrivals) {
        if (arrival->is_monitored())
          arrival->report(resource, start, activity);
      }
    }

    void report(Arrival* arrival) const {
      for (const auto& itr : restime)
        arrival->report(itr.first, itr.second.start,
                        itr.second.activity - status.busy_until + sim->now());
    }

    void update_activity(double value) {
      Arrival::update_activity(value);
      for (auto arrival : arrivals)
        arrival->update_activity(value);
    }

    void set_remaining(double value) {
      Arrival::set_remaining(value);
      for (auto arrival : arrivals)
        arrival->set_remaining(value);
    }

    void set_busy(double value) {
      Arrival::set_busy(value);
      for (auto arrival : arrivals)
        arrival->set_busy(value);
    }
  };

} // namespace simmer

#endif
