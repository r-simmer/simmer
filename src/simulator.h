#ifndef SIMULATOR_H
#define SIMULATOR_H

#include "simmer.h"
#include "monitor.h"
#include "process.h"
#include "resource.h"

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

  /**
   * Constructor.
   * @param name    simulator name
   * @param verbose verbose flag
   * @param mon     monitoring object
   */
  Simulator(const std::string& name, bool verbose)
    : name(name), verbose(verbose), mon(new MemoryMon()),
      now_(0), process_(NULL), b_count(0) {}

  ~Simulator() {
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
    delete mon;
  }

  /**
   * Reset the simulation: time, event queue, resources, processes and statistics.
   */
  void reset() {
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
  RData peek(int steps) const {
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

  /**
   * Process the next event. Only one step, a giant leap for mankind.
   */
  bool step(double until = -1) {
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

  /**
   * Executes steps until the given criterion is met.
   * @param   until   time of ending
   */
  void run(double until) {
    size_t nsteps = 0;
    while (step(until))
      if (++nsteps % 100000 == 0) Rcpp::checkUserInterrupt();
  }

  /**
   * Add a generator of arrivals to the simulator.
   * @param   name_prefix     prefix for the arrival names
   * @param   trj             a user-defined R trajectory
   * @param   dist            a user-defined R function that provides random numbers
   * @param   mon             monitoring level
   * @param   priority        arrival priority
   * @param   preemptible     maximum priority that cannot cause preemption (>=priority)
   * @param   restart         whether activity must be restarted after preemption
   */
  bool add_generator(const std::string& name_prefix, REnv trj, RFn dist,
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

  /**
   * Attach arrivals from a data frame.
   * @param   name_prefix     prefix for the arrival names
   * @param   trj             a user-defined R trajectory
   * @param   data            a user-supplied data frame
   * @param   mon             monitoring level
   * @param   time            column name
   * @param   attrs           column names for attributes
   * @param   priority        column name
   * @param   preemptible     column name
   * @param   restart         column name
   */
  bool add_dataframe(const std::string& name_prefix, REnv trj, RData data, int mon,
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

  /**
   * Add a resource to the simulator.
   * @param   name              the name
   * @param   capacity          server capacity (-1 means infinity)
   * @param   queue_size        room in the queue (-1 means infinity)
   * @param   mon               whether this entity must be monitored
   * @param   preemptive        whether the resource is preemptive
   * @param   preempt_order     fifo or lifo
   * @param   queue_size_strict whether the queue size is a hard limit
   */
  bool add_resource(const std::string& name, int capacity, int queue_size, bool mon,
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

  /**
   * Add a process that manages the capacity or the queue_size of a resource
   * given a scheduling.
   * @param   name      the resource name
   * @param   param     "capacity" or "queue_size"
   * @param   duration  vector of durations until the next value change
   * @param   value     vector of values
   */
  bool add_resource_manager(const std::string& name, const std::string& param,
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

  /**
   * Get a source by name.
   */
  Source* get_source(const std::string& name) const {
    EntMap::const_iterator search = process_map.find(name);
    if (search == process_map.end())
      Rcpp::stop("source '%s' not found (typo?)", name);
    return static_cast<Source*>(search->second);
  }

  /**
   * Get a resource by name.
   */
  Resource* get_resource(const std::string& name) const {
    EntMap::const_iterator search = resource_map.find(name);
    if (search == resource_map.end())
      Rcpp::stop("resource '%s' not found (typo?)", name);
    return static_cast<Resource*>(search->second);
  }

  Arrival* get_running_arrival() const {
    Arrival* arrival = dynamic_cast<Arrival*>(process_);
    if (!arrival)
      Rcpp::stop("there is no arrival running");
    return arrival;
  }

  void record_ongoing(bool per_resource) const {
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

  Batched** get_batch(Activity* ptr, const std::string& id) {
    if (id.size()) {
      if (namedb_map.find(id) == namedb_map.end())
        namedb_map[id] = NULL;
      return &namedb_map[id];
    } else {
      if (unnamedb_map.find(ptr) == unnamedb_map.end())
        unnamedb_map[ptr] = NULL;
      return &unnamedb_map[ptr];
    }
  }

  size_t get_batch_count() { return b_count++; }

  void broadcast(const VEC<std::string>& signals) {
    foreach_ (const std::string& signal, signals) {
      foreach_ (const HandlerMap::value_type& itr, signal_map[signal]) {
        if (!itr.second.first)
          continue;
        Task* task = new Task(this, "Handler", itr.second.second, PRIORITY_SIGNAL);
        task->activate();
      }
    }
  }
  void subscribe(const std::string& signal, Arrival* arrival, Fn<void()> handler) {
    signal_map[signal][arrival] = std::make_pair(true, handler);
    arrival_map[arrival].emplace(signal);
  }
  void subscribe(const VEC<std::string>& signals, Arrival* arrival, Fn<void()> handler) {
    foreach_ (const std::string& signal, signals)
      subscribe(signal, arrival, handler);
  }
  void subscribe(Arrival* arrival) {
    foreach_ (const std::string& signal, arrival_map[arrival])
      signal_map[signal][arrival].first = true;
  }
  void unsubscribe(const std::string& signal, Arrival* arrival) {
    signal_map[signal].erase(arrival);
    arrival_map[arrival].erase(signal);
  }
  void unsubscribe(const VEC<std::string>& signals, Arrival* arrival) {
    foreach_ (const std::string& signal, signals)
      unsubscribe(signal, arrival);
  }
  void unsubscribe(Arrival* arrival) {
    foreach_ (const std::string& signal, arrival_map[arrival])
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
    foreach_ (const std::string& signal, arrival_map[arrival])
      signal_map[signal].erase(arrival);
    arrival_map.erase(arrival);
  }

private:
  double now_;              /**< simulation time */
  Process* process_;        /**< running process */
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
};

#endif
