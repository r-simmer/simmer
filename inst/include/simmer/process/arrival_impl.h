// Copyright (C) 2016 Bart Smeets and Iñaki Ucar
// Copyright (C) 2016-2020 Iñaki Ucar
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

#ifndef simmer__process_arrival_impl_h
#define simmer__process_arrival_impl_h

#include <simmer/process/arrival.h>
#include <simmer/process/batched.h>
#include <simmer/resource.h>

namespace simmer {

  inline void Arrival::terminate(bool finished) {
    foreach_ (ResVec::value_type& itr, resources) {
      if (itr->is_waiting(this))
        itr->remove(this);
    }
    if (!finished && dropout) {
      activity = dropout;
      sim->schedule(0, this, priority);
      return;
    }

    foreach_ (ResVec::value_type& itr, resources) {
      Rcpp::warning("'%s': leaving without releasing '%s'", name, itr->name);
      itr->remove(this, true);
    }
    unset_remaining();
    if (is_monitored() >= 1 && !dynamic_cast<Batched*>(this))
      sim->mon->record_end(name, lifetime.start, sim->now(), lifetime.activity, finished);
    delete this;
  }

  inline double Arrival::get_start(const std::string& name) {
    double start = restime[name].start;
    if (batch) {
      double up = batch->get_start(name);
      if (up >= 0 && (start < 0 || up < start))
        start = up;
    }
    return start;
  }

  inline void Arrival::register_entity(Resource* ptr) {
    if (!ptr) Rcpp::stop("illegal register of arrival '%s'", name); // # nocov
    if (is_monitored()) {
      restime[ptr->name].start = sim->now();
      restime[ptr->name].activity = 0;
    }
    resources.push_back(ptr);
  }

  inline void Arrival::unregister_entity(Resource* ptr) {
    ResVec::iterator search = std::find(resources.begin(), resources.end(), ptr);
    if (!ptr || search == resources.end())
      Rcpp::stop("illegal unregister of arrival '%s'", name); // # nocov
    if (is_monitored())
      report(ptr->name);
    resources.erase(search);
  }

  inline void Arrival::leave_resources(bool keep_seized) {
    if (status.busy_until > sim->now())
      unset_busy(sim->now());
    unset_remaining();
    foreach_ (ResVec::value_type& itr, resources) {
      if (itr->is_waiting(this))
        --paused;
      if (!keep_seized || itr->is_waiting(this))
        itr->remove(this);
    }
  }

  inline void Arrival::renege(Activity* next, bool keep_seized) {
    timer = NULL;
    cancel_renege();
    if (batch && !batch->remove(this))
      return;
    leave_resources(keep_seized);
    deactivate();
    if (next) {
      activity = next;
      activate();
    } else terminate(false);
  }

} // namespace simmer

#endif
