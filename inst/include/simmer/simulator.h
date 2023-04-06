// Copyright (C) 2014-2015 Bart Smeets
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

#ifndef simmer__simulator_h
#define simmer__simulator_h

#include <simmer/common.h>
#include <simmer/monitor.h>

namespace simmer {

  class Entity;
  class Resource;
  class Process;
  class Source;
  class Arrival;
  class Batched;
  class Activity;

  /**
   * The simulator.
   */
  class Simulator {
    /**
     * Event container. Encapsulates future processes in the event queue.
     */
    struct Event {
      double time;
      Process* process;
      int priority;

      Event(double time, Process* process, int priority):
        time(time), process(process), priority(priority) {}

      bool operator<(const Event& other) const {
        if (time == other.time)
          return priority < other.priority;
        return time < other.time;
      }
    };

    typedef MSET<Event> PQueue;
    typedef UMAP<Process*, PQueue::iterator> EvMap;
    typedef MAP<std::string, Entity*> EntMap;
    typedef UMAP<Arrival*, USET<std::string> > ArrMap;
    typedef UMAP<std::string, Batched*> NamBMap;
    typedef UMAP<Activity*, Batched*> UnnBMap;
    typedef std::pair<bool, Fn<void()> > Handler;
    typedef UMAP<Arrival*, Handler> HandlerMap;
    typedef UMAP<std::string, HandlerMap> SigMap;

  public:
    std::string name;
    bool verbose;
    Monitor* mon;
    int log_level;

    /**
     * Constructor.
     * @param name      simulator name
     * @param verbose   verbose flag
     * @param mon       monitoring object
     * @param log_level debugging level (for activities)
     */
    Simulator(const std::string& name, bool verbose, Monitor* mon, int log_level)
      : name(name), verbose(verbose), mon(mon), log_level(log_level), now_(0),
        process_(NULL), stop_(false), b_count(0) {}

    ~Simulator();

    /**
     * Reset the simulation: time, event queue, resources, processes and statistics.
     */
    void reset();

    double now() const { return now_; }

    /**
     * Schedule a future event.
     * @param   delay     delay from now()
     * @param   process   the process to schedule
     * @param   priority  additional key to execute releases before seizes if they coincide
     */
    void schedule(double delay, Process* process, int priority=0) {
      event_map[process] = event_queue.emplace(now_ + delay, process, priority);
    }

    void unschedule(Process* process) {
      event_queue.erase(event_map[process]);
      event_map.erase(process);
    }

    bool is_scheduled(Process* process) {
      if (event_map.find(process) != event_map.end())
        return true;
      return false;
    }

    /**
     * Look for future events.
     */
    RData peek(int steps) const;

    /**
     * Executes steps until the given criterion is met.
     * @param   until   time of ending
     */
    void run(double until) {
      size_t nsteps = 0;
      while (_step(until))
        if (++nsteps % 100000 == 0) Rcpp::checkUserInterrupt();
      mon->flush();
    }

    void step(unsigned int n = 1) {
      size_t nsteps = 0;
      while (n-- && _step())
        if (++nsteps % 100000 == 0) Rcpp::checkUserInterrupt();
      mon->flush();
    }

    void request_stop() { stop_ = true; }

    void print(const std::string& e_type,      const std::string& e_name,
               const std::string& a_type = "", const std::string& a_name = "",
               const std::string& trail = "",  bool flush=true) const
    {
      Rcpp::Rcout <<
        FMT(10, right) << now_ << " |" <<
        FMT(12, right) << e_type + ": " << FMT(17, left) << e_name << "|" <<
        FMT(12, right) << a_type + ": " << FMT(17, left) << a_name << "| " << trail;
      if (flush) Rcpp::Rcout << std::endl;
    }

    bool add_process(Process* process);
    bool add_resource(Resource* resource);
    Process* get_process(const std::string& name) const;
    Source* get_source(const std::string& name) const;
    Resource* get_resource(const std::string& name) const;
    Arrival* get_running_arrival() const;
    RData get_ongoing(bool per_resource) const;

    VEC<std::string> get_resources() const {
      VEC<std::string> out;
      for (const auto& itr : resource_map)
        out.push_back(itr.first);
      return out;
    }

    void set_batch(Activity* ptr, const std::string& id, Batched* arr) {
      if (id.size())
        namedb_map[id] = arr;
      else unnamedb_map[ptr] = arr;
    }

    Batched* get_batch(Activity* ptr, const std::string& id) {
      if (id.size())
        return namedb_map[id];
      return unnamedb_map[ptr];
    }

    size_t get_batch_count() { return b_count++; }

    void broadcast(const VEC<std::string>& signals);
    void subscribe(const std::string& signal, Arrival* arrival, Fn<void()> handler) {
      signal_map[signal][arrival] = std::make_pair(true, handler);
      arrival_map[arrival].emplace(signal);
    }
    void subscribe(const VEC<std::string>& signals, Arrival* arrival, Fn<void()> handler) {
      for (const auto& signal : signals)
        subscribe(signal, arrival, handler);
    }
    void subscribe(Arrival* arrival) {
      for (const auto& signal : arrival_map[arrival])
        signal_map[signal][arrival].first = true;
    }
    void unsubscribe(const std::string& signal, Arrival* arrival) {
      signal_map[signal].erase(arrival);
      arrival_map[arrival].erase(signal);
    }
    void unsubscribe(const VEC<std::string>& signals, Arrival* arrival) {
      for (const auto& signal : signals)
        unsubscribe(signal, arrival);
    }
    void unsubscribe(Arrival* arrival) {
      for (const auto& signal : arrival_map[arrival])
        signal_map[signal][arrival].first = false;
    }

    void set_attribute(const std::string& key, double value) {
      attributes[key] = value;
      mon->record_attribute(now_, "", key, value);
    }
    double get_attribute(const std::string& key) const {
      Attr::const_iterator search = attributes.find(key);
      if (search == attributes.end())
        return NA_REAL;
      return search->second;
    }

    void register_arrival(Arrival* arrival) { arrival_map[arrival]; }
    void unregister_arrival(Arrival* arrival) {
      for (const auto& signal : arrival_map[arrival])
        signal_map[signal].erase(arrival);
      arrival_map.erase(arrival);
    }

  private:
    double now_;              /**< simulation time */
    Process* process_;        /**< running process */
    bool stop_;               /**< stop flag */
    PQueue event_queue;       /**< the event queue */
    EntMap resource_map;      /**< map of resources */
    EntMap process_map;       /**< map of processes */
    EvMap event_map;          /**< map of pending events */
    ArrMap arrival_map;       /**< map of ongoing arrivals */
    NamBMap namedb_map;       /**< map of named batches */
    UnnBMap unnamedb_map;     /**< map of unnamed batches */
    size_t b_count;           /**< unnamed batch counter */
    SigMap signal_map;        /**< map of arrivals subscribed to signals */
    Attr attributes;          /**< user-defined (key, value) pairs */

    /**
     * Process the next event. Only one step, a giant leap for mankind.
     */
    bool _step(double until = -1);

    std::string format(Process* process, const char* append);
  };

} // namespace simmer

#endif
