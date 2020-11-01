// Copyright (C) 2014-2015 Bart Smeets
// Copyright (C) 2015-2016 Bart Smeets and Iñaki Ucar
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

#ifndef simmer__simulator_impl_h
#define simmer__simulator_impl_h

#include <simmer/simulator.h>

#include <simmer/process.h>
#include <simmer/process/task.h>
#include <simmer/process/manager.h>
#include <simmer/process/source.h>
#include <simmer/process/generator.h>
#include <simmer/process/datasrc.h>
#include <simmer/process/arrival.h>
#include <simmer/process/batched.h>

#include <simmer/resource.h>
#include <simmer/resource/types.h>
#include <simmer/resource/priority.h>
#include <simmer/resource/preemptive.h>

#include <simmer/monitor/memory.h>

namespace simmer {

  inline Simulator::~Simulator() {
    foreach_ (EntMap::value_type& itr, resource_map)
      delete itr.second;
    foreach_ (PQueue::value_type& itr, event_queue)
      if (dynamic_cast<Arrival*>(itr.process)) delete itr.process;
    foreach_ (EntMap::value_type& itr, process_map)
      delete itr.second;
    foreach_ (NamBMap::value_type& itr, namedb_map)
      if (itr.second) delete itr.second;
    foreach_ (UnnBMap::value_type& itr, unnamedb_map)
      if (itr.second) delete itr.second;
  }

  inline void Simulator::reset() {
    now_ = 0;

    foreach_ (EntMap::value_type& itr, resource_map)
      static_cast<Resource*>(itr.second)->reset();
    foreach_ (PQueue::value_type& itr, event_queue)
      if (dynamic_cast<Arrival*>(itr.process)) delete itr.process;

    event_queue.clear();
    event_map.clear();
    attributes.clear();
    mon->clear();

    foreach_ (EntMap::value_type& itr, process_map) {
      static_cast<Process*>(itr.second)->reset();
      static_cast<Process*>(itr.second)->activate();
    }

    foreach_ (NamBMap::value_type& itr, namedb_map)
      if (itr.second) delete itr.second;
    foreach_ (UnnBMap::value_type& itr, unnamedb_map)
      if (itr.second) delete itr.second;

    namedb_map.clear();
    unnamedb_map.clear();
    arrival_map.clear();
    signal_map.clear();
    b_count = 0;
  }

  inline RData Simulator::peek(int steps) const {
    VEC<double> time;
    VEC<std::string> process;
    if (steps) {
      foreach_ (const PQueue::value_type& itr, event_queue) {
        time.push_back(itr.time);
        process.push_back(itr.process->name);
        if (!--steps) break;
      }
    }
    return RData::create(
      Rcpp::Named("time")             = time,
      Rcpp::Named("process")          = process,
      Rcpp::Named("stringsAsFactors") = false
    );
  }

  inline bool Simulator::add_process(Process* process) {
    if (process_map.find(process->name) != process_map.end()) {
      Rcpp::warning("process '%s' already defined", process->name);
      return false;
    }
    process_map[process->name] = process;
    process->activate();
    return true;
  }

  inline bool Simulator::add_resource(Resource* resource) {
    if (resource_map.find(resource->name) != resource_map.end()) {
      Rcpp::warning("resource '%s' already defined", resource->name);
      return false;
    }
    resource_map[resource->name] = resource;
    return true;
  }

  inline Process* Simulator::get_process(const std::string& name) const {
    EntMap::const_iterator search = process_map.find(name);
    if (search == process_map.end())
      Rcpp::stop("process '%s' not found (typo?)", name);
    return static_cast<Process*>(search->second);
  }

  inline Source* Simulator::get_source(const std::string& name) const {
    Source* src = dynamic_cast<Source*>(get_process(name));
    if (!src)
      Rcpp::stop("process '%s' exists, but it is not a source", name);
    return src;
  }

  inline Resource* Simulator::get_resource(const std::string& name) const {
    EntMap::const_iterator search = resource_map.find(name);
    if (search == resource_map.end())
      Rcpp::stop("resource '%s' not found (typo?)", name);
    return static_cast<Resource*>(search->second);
  }

  inline Arrival* Simulator::get_running_arrival() const {
    Arrival* arrival = dynamic_cast<Arrival*>(process_);
    if (!arrival)
      Rcpp::stop("there is no arrival running");
    return arrival;
  }

  inline RData Simulator::get_ongoing(bool per_resource) const {
    MemMonitor mon;
    foreach_ (const ArrMap::value_type& itr1, arrival_map) {
      if (dynamic_cast<Batched*>(itr1.first) || !itr1.first->is_monitored())
        continue;
      if (!per_resource)
        mon.record_end(itr1.first->name, itr1.first->get_start(), R_NaReal, R_NaReal, false);
      else foreach_ (const EntMap::value_type& itr2, resource_map) {
        double start = itr1.first->get_start(itr2.second->name);
        if (start < 0)
          continue;
        mon.record_release(itr1.first->name, start, R_NaReal, R_NaReal, itr2.second->name);
      }
    }
    return mon.get_arrivals(per_resource);
  }

  inline void Simulator::broadcast(const VEC<std::string>& signals) {
    foreach_ (const std::string& signal, signals) {
      foreach_ (const HandlerMap::value_type& itr, signal_map[signal]) {
        if (!itr.second.first)
          continue;
        if (is_scheduled(itr.first))
          unsubscribe(itr.first);
        Task* task = new Task(this, "Handler", itr.second.second, PRIORITY_SIGNAL);
        task->activate();
      }
    }
  }

  inline bool Simulator::_step(double until) {
    if (event_queue.empty())
      return false;
    PQueue::iterator ev = event_queue.begin();
    if (until >= 0 && until <= ev->time) {
      if (until > now_)
        now_ = until;
      return false;
    }
    now_ = ev->time;
    process_ = ev->process;
    event_map.erase(process_);
    try {
      process_->run();
    } catch (std::exception &ex) {
      throw Rcpp::exception(format(process_, ex.what()).c_str(), false);
    }
    event_queue.erase(ev);
    if (stop_) {
      Rf_warningcall_immediate(R_NilValue, format(
          process_, "execution stopped by a breakpoint").c_str());
      return stop_ = false;
    }
    process_ = NULL;
    return true;
  }

  inline std::string Simulator::format(Process* process, const char* append) {
    std::stringstream context;
    if (Arrival* arrival = dynamic_cast<Arrival*>(process)) {
      context << " in [";
      if (Activity* prev = arrival->get_activity()->get_prev())
        context << prev->name;
      context << "]->" << arrival->get_activity()->name << "->[";
      if (Activity* next = arrival->get_activity()->get_next())
        context << next->name;
      context << "]";
    }
    return tfm::format(
      "'%s' at %.2f%s:\n %s", process->name, now_, context.str(), append);
  }

} // namespace simmer

#endif
