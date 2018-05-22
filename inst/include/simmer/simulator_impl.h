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
    foreach_ (EntMap::value_type& itr, process_map) {
      static_cast<Process*>(itr.second)->reset();
      static_cast<Process*>(itr.second)->activate();
    }
    foreach_ (NamBMap::value_type& itr, namedb_map)
      if (itr.second) delete itr.second;
    foreach_ (UnnBMap::value_type& itr, unnamedb_map)
      if (itr.second) delete itr.second;
    arrival_map.clear();
    namedb_map.clear();
    unnamedb_map.clear();
    b_count = 0;
    signal_map.clear();
    attributes.clear();
    mon->clear();
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

  inline bool Simulator::add_generator(const std::string& name_prefix, REnv trj, RFn dist,
                     int mon, int priority, int preemptible, bool restart)
  {
    if (process_map.find(name_prefix) != process_map.end()) {
      Rcpp::warning("process '%s' already defined", name_prefix);
      return false;
    }
    Generator* gen = new Generator(this, name_prefix, mon, trj, dist,
                                   Order(priority, preemptible, restart));
    process_map[name_prefix] = gen;
    gen->activate();
    return true;
  }

  inline bool Simulator::add_dataframe(const std::string& name_prefix, REnv trj, RData data, int mon,
                     int batch, const std::string& time, const VEC<std::string>& attrs,
                     const OPT<std::string>& priority,
                     const OPT<std::string>& preemptible,
                     const OPT<std::string>& restart)
  {
    if (process_map.find(name_prefix) != process_map.end()) {
      Rcpp::warning("process '%s' already defined", name_prefix);
      return false;
    }
    DataSrc* gen = new DataSrc(this, name_prefix, mon, trj, data, batch, time,
                               attrs, priority, preemptible, restart);
    process_map[name_prefix] = gen;
    gen->activate();
    return true;
  }

  inline bool Simulator::add_resource(const std::string& name, int capacity, int queue_size, bool mon,
                    bool preemptive, const std::string& preempt_order, bool queue_size_strict)
  {
    if (resource_map.find(name) != resource_map.end()) {
      Rcpp::warning("resource '%s' already defined", name);
      return false;
    }
    Resource* res;
    if (!preemptive) {
      res = new PriorityRes<FIFO>(this, name, mon, capacity,
                                  queue_size, queue_size_strict);
    } else {
      if (preempt_order.compare("fifo") == 0)
        res = new PreemptiveRes<FIFO>(this, name, mon, capacity,
                                      queue_size, queue_size_strict);
      else
        res = new PreemptiveRes<LIFO>(this, name, mon, capacity,
                                      queue_size, queue_size_strict);
    }
    resource_map[name] = res;
    return true;
  }

  inline bool Simulator::add_resource_manager(const std::string& name, const std::string& param,
                            const VEC<double>& duration, const VEC<int>& value, int period)
  {
    if (process_map.find(name) != process_map.end())
      Rcpp::stop("process '%s' already defined", name);
    EntMap::iterator search = resource_map.find(name);
    if (search == resource_map.end())
      Rcpp::stop("resource '%s' not found (typo?)", name);
    Resource* res = static_cast<Resource*>(search->second);
    Manager* manager;
    if (param.compare("capacity") == 0)
      manager = new Manager(this, name, param, duration, value, period,
                            BIND(&Resource::set_capacity, res, _1));
    else
      manager = new Manager(this, name, param, duration, value, period,
                            BIND(&Resource::set_queue_size, res, _1));
    process_map[name + "_" + param] = manager;
    manager->activate();
    return true;
  }

  inline Source* Simulator::get_source(const std::string& name) const {
    EntMap::const_iterator search = process_map.find(name);
    if (search == process_map.end())
      Rcpp::stop("source '%s' not found (typo?)", name);
    return static_cast<Source*>(search->second);
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

  inline void Simulator::record_ongoing(bool per_resource) const {
    foreach_ (const ArrMap::value_type& itr1, arrival_map) {
      if (dynamic_cast<Batched*>(itr1.first) || !itr1.first->is_monitored())
        continue;
      if (!per_resource)
        mon->record_end(itr1.first->name, itr1.first->get_start(), R_NaReal, R_NaReal, false);
      else foreach_ (const EntMap::value_type& itr2, resource_map) {
        double start = itr1.first->get_start(itr2.second->name);
        if (start < 0)
          continue;
        mon->record_release(itr1.first->name, start, R_NaReal, R_NaReal, itr2.second->name);
      }
    }
  }

  inline void Simulator::broadcast(const VEC<std::string>& signals) {
    foreach_ (const std::string& signal, signals) {
      foreach_ (const HandlerMap::value_type& itr, signal_map[signal]) {
        if (!itr.second.first)
          continue;
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
    event_map.erase(ev->process);
    process_->run();
    process_ = NULL;
    event_queue.erase(ev);
    return true;
  }

} // namespace simmer

#endif
