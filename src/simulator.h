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
  typedef UMAP<std::string, Entity*> EntMap;
  typedef UMAP<Arrival*, USET<std::string> > ArrMap;
  typedef UMAP<std::string, Batched*> NamBMap;
  typedef UMAP<Activity*, Batched*> UnnBMap;
  typedef std::pair<bool, BIND(void)> Handler;
  typedef UMAP<Arrival*, Handler> HandlerMap;
  typedef UMAP<std::string, HandlerMap> SigMap;

public:
  std::string name;
  bool verbose;

  /**
   * Constructor.
   * @param name    simulator name
   * @param verbose verbose flag
   */
  Simulator(std::string name, bool verbose)
    : name(name), verbose(verbose), now_(0), b_count(0) {}

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
  }

  /**
   * Reset the simulation: time, event queue, resources, processes and statistics.
   */
  void reset() {
    now_ = 0;
    foreach_ (EntMap::value_type& itr, resource_map)
      ((Resource*)itr.second)->reset();
    foreach_ (PQueue::value_type& itr, event_queue)
      if (dynamic_cast<Arrival*>(itr.process)) delete itr.process;
    event_queue.clear();
    event_map.clear();
    foreach_ (EntMap::value_type& itr, process_map) {
      ((Process*)itr.second)->reset();
      ((Process*)itr.second)->activate();
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
    mon_arr_traj.clear();
    mon_arr_res.clear();
    mon_attributes.clear();
    mon_resources.clear();
  }

  double now() { return now_; }

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

  /**
   * Look for future events.
   */
  Rcpp::DataFrame peek(int steps) {
    VEC<double> time;
    VEC<std::string> process;
    if (steps) {
      foreach_ (PQueue::value_type& itr, event_queue) {
        time.push_back(itr.time);
        process.push_back(itr.process->name);
        if (!--steps) break;
      }
    }
    return Rcpp::DataFrame::create(
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
    event_map.erase(ev->process);
    ev->process->run();
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
   * @param   dis             a user-defined R function that provides random numbers
   * @param   mon             monitoring level
   * @param   priority        arrival priority
   * @param   preemptible     maximum priority that cannot cause preemption (>=priority)
   * @param   restart         whether activity must be restarted after preemption
   */
  bool add_generator(std::string name_prefix, Rcpp::Environment trj, Rcpp::Function dist,
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
   * Add a resource to the simulator.
   * @param   name              the name
   * @param   capacity          server capacity (-1 means infinity)
   * @param   queue_size        room in the queue (-1 means infinity)
   * @param   mon               whether this entity must be monitored
   * @param   preemptive        whether the resource is preemptive
   * @param   preempt_order     fifo or lifo
   * @param   queue_size_strict whether the queue size is a hard limit
   */
  bool add_resource(std::string name, int capacity, int queue_size, bool mon,
                    bool preemptive, std::string preempt_order, bool queue_size_strict)
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
  bool add_resource_manager(std::string name, std::string param,
                            VEC<double> duration, VEC<int> value, int period)
  {
    if (process_map.find(name) != process_map.end())
      Rcpp::stop("process '%s' already defined", name);
    EntMap::iterator search = resource_map.find(name);
    if (search == resource_map.end())
      Rcpp::stop("resource '%s' not found (typo?)", name);
    Resource* res = (Resource*)search->second;
    Manager* manager;
    if (param.compare("capacity") == 0)
      manager = new Manager(this, name, param, duration, value, period,
                            boost::bind(&Resource::set_capacity, res, _1));
    else
      manager = new Manager(this, name, param, duration, value, period,
                            boost::bind(&Resource::set_queue_size, res, _1));
    process_map[name + "_" + param] = manager;
    manager->activate();
    return true;
  }

  /**
   * Get a generator by name.
   */
  Generator* get_generator(std::string name) {
    EntMap::iterator search = process_map.find(name);
    if (search == process_map.end())
      Rcpp::stop("generator '%s' not found (typo?)", name);
    return (Generator*)search->second;
  }

  /**
   * Get a resource by name.
   */
  Resource* get_resource(std::string name) {
    EntMap::iterator search = resource_map.find(name);
    if (search == resource_map.end())
      Rcpp::stop("resource '%s' not found (typo?)", name);
    return (Resource*)search->second;
  }

  Batched** get_batch(Activity* ptr, std::string id) {
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

  void broadcast(VEC<std::string> signals) {
    foreach_ (std::string signal, signals) {
      foreach_ (HandlerMap::value_type& itr, signal_map[signal]) {
        if (!itr.second.first)
          continue;
        Task* task = new Task(this, "Handler", itr.second.second);
        task->activate();
      }
    }
  }
  void subscribe(std::string signal, Arrival* arrival, BIND(void) handler) {
    signal_map[signal][arrival] = std::make_pair(true, handler);
    arrival_map[arrival].emplace(signal);
  }
  void subscribe(VEC<std::string> signals, Arrival* arrival, BIND(void) handler) {
    foreach_ (std::string signal, signals)
      subscribe(signal, arrival, handler);
  }
  void subscribe(Arrival* arrival) {
    foreach_ (std::string signal, arrival_map[arrival])
      signal_map[signal][arrival].first = true;
  }
  void unsubscribe(std::string signal, Arrival* arrival) {
    signal_map[signal].erase(arrival);
    arrival_map[arrival].erase(signal);
  }
  void unsubscribe(VEC<std::string> signals, Arrival* arrival) {
    foreach_ (std::string signal, signals)
      unsubscribe(signal, arrival);
  }
  void unsubscribe(Arrival* arrival) {
    foreach_ (std::string signal, arrival_map[arrival])
      signal_map[signal][arrival].first = false;
  }

  void set_attribute(std::string key, double value) {
    attributes[key] = value;
    record_attribute("", key, value);
  }
  Attr* get_attributes() { return &attributes; }

  void register_arrival(Arrival* arrival) { arrival_map[arrival]; }
  void unregister_arrival(Arrival* arrival) {
    foreach_ (std::string signal, arrival_map[arrival])
      signal_map[signal].erase(arrival);
    arrival_map.erase(arrival);
  }

  /**
   * Record monitoring data.
   */
  void record_end(std::string name, double start, double activity, bool finished) {
    mon_arr_traj.insert("name",           name);
    mon_arr_traj.insert("start_time",     start);
    mon_arr_traj.insert("end_time",       now_);
    mon_arr_traj.insert("activity_time",  activity);
    mon_arr_traj.insert("finished",       finished);
  }
  void record_release(std::string name, double start, double activity, std::string resource) {
    mon_arr_res.insert("name",            name);
    mon_arr_res.insert("start_time",      start);
    mon_arr_res.insert("end_time",        now_);
    mon_arr_res.insert("activity_time",   activity);
    mon_arr_res.insert("resource",        resource);
  }
  void record_attribute(std::string name, std::string key, double value) {
    mon_attributes.insert("time",         now_);
    mon_attributes.insert("name",         name);
    mon_attributes.insert("key",          key);
    mon_attributes.insert("value",        value);
  }
  void record_resource(std::string name, int server_count, int queue_count,
                       int capacity, int queue_size) {
    mon_resources.insert("resource",      name);
    mon_resources.insert("time",          now_);
    mon_resources.insert("server",        server_count);
    mon_resources.insert("queue",         queue_count);
    mon_resources.insert("capacity",      capacity);
    mon_resources.insert("queue_size",    queue_size);
  }

  /**
   * Get monitoring data.
   */
  Rcpp::DataFrame get_mon_arrivals(bool per_resource, bool ongoing) {
    if (!per_resource) {
      VEC<std::string> name             = mon_arr_traj.get<std::string>("name");
      VEC<double> start_time            = mon_arr_traj.get<double>("start_time");
      VEC<double> end_time              = mon_arr_traj.get<double>("end_time");
      VEC<double> activity_time         = mon_arr_traj.get<double>("activity_time");
      Rcpp::LogicalVector finished      = Rcpp::wrap(mon_arr_traj.get<bool>("finished"));
      if (ongoing) {
        foreach_ (ArrMap::value_type& itr, arrival_map) {
          if (!itr.first->is_monitored())
            continue;
          name.push_back(itr.first->name);
          start_time.push_back(itr.first->get_start());
          end_time.push_back(R_NaReal);
          activity_time.push_back(R_NaReal);
          finished.push_back(R_NaInt);
        }
      }
      return Rcpp::DataFrame::create(
        Rcpp::Named("name")             = name,
        Rcpp::Named("start_time")       = start_time,
        Rcpp::Named("end_time")         = end_time,
        Rcpp::Named("activity_time")    = activity_time,
        Rcpp::Named("finished")         = finished,
        Rcpp::Named("stringsAsFactors") = false
      );
    } else {
      VEC<std::string> name             = mon_arr_res.get<std::string>("name");
      VEC<double> start_time            = mon_arr_res.get<double>("start_time");
      VEC<double> end_time              = mon_arr_res.get<double>("end_time");
      VEC<double> activity_time         = mon_arr_res.get<double>("activity_time");
      VEC<std::string> resource         = mon_arr_res.get<std::string>("resource");
      if (ongoing) {
        foreach_ (ArrMap::value_type& itr1, arrival_map) {
          if (!itr1.first->is_monitored())
            continue;
          foreach_ (EntMap::value_type& itr2, resource_map) {
            double start = itr1.first->get_start(itr2.second->name);
            if (start < 0)
              continue;
            name.push_back(itr1.first->name);
            start_time.push_back(start);
            end_time.push_back(R_NaReal);
            activity_time.push_back(R_NaReal);
            resource.push_back(itr2.second->name);
          }
        }
      }
      return Rcpp::DataFrame::create(
        Rcpp::Named("name")             = name,
        Rcpp::Named("start_time")       = start_time,
        Rcpp::Named("end_time")         = end_time,
        Rcpp::Named("activity_time")    = activity_time,
        Rcpp::Named("resource")         = resource,
        Rcpp::Named("stringsAsFactors") = false
      );
    }
  }
  Rcpp::DataFrame get_mon_attributes() {
    return Rcpp::DataFrame::create(
      Rcpp::Named("time")             = mon_attributes.get<double>("time"),
      Rcpp::Named("name")             = mon_attributes.get<std::string>("name"),
      Rcpp::Named("key")              = mon_attributes.get<std::string>("key"),
      Rcpp::Named("value")            = mon_attributes.get<double>("value"),
      Rcpp::Named("stringsAsFactors") = false
    );
  }
  Rcpp::DataFrame get_mon_resources() {
    return Rcpp::DataFrame::create(
      Rcpp::Named("resource")         = mon_resources.get<std::string>("resource"),
      Rcpp::Named("time")             = mon_resources.get<double>("time"),
      Rcpp::Named("server")           = mon_resources.get<int>("server"),
      Rcpp::Named("queue")            = mon_resources.get<int>("queue"),
      Rcpp::Named("capacity")         = mon_resources.get<int>("capacity"),
      Rcpp::Named("queue_size")       = mon_resources.get<int>("queue_size"),
      Rcpp::Named("stringsAsFactors") = false
    );
  }
  Rcpp::DataFrame get_mon_resources_counts() {
    return Rcpp::DataFrame::create(
      Rcpp::Named("resource")         = mon_resources.get<std::string>("resource"),
      Rcpp::Named("time")             = mon_resources.get<double>("time"),
      Rcpp::Named("server")           = mon_resources.get<int>("server"),
      Rcpp::Named("queue")            = mon_resources.get<int>("queue"),
      Rcpp::Named("stringsAsFactors") = false
    );
  }
  Rcpp::DataFrame get_mon_resources_limits() {
    return Rcpp::DataFrame::create(
      Rcpp::Named("resource")         = mon_resources.get<std::string>("resource"),
      Rcpp::Named("time")             = mon_resources.get<double>("time"),
      Rcpp::Named("server")           = mon_resources.get<int>("capacity"),
      Rcpp::Named("queue")            = mon_resources.get<int>("queue_size")
    );
  }

private:
  double now_;              /**< simulation time */
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
  Monitor mon_arr_traj;     /**< arrival statistics per trajectory */
  Monitor mon_arr_res;      /**< arrival statistics per resource */
  Monitor mon_attributes;   /**< attribute statistics */
  Monitor mon_resources;    /**< resource statistics */
};

#endif
