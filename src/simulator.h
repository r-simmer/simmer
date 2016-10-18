#ifndef SIMULATOR_H
#define SIMULATOR_H

#include "simmer.h"
#include "stats.h"
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
  typedef boost::function<void ()> Bind;
  typedef UMAP<Arrival*, Bind> HandlerMap;
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
    foreach_ (PQueue::value_type& itr, event_queue)
      if (dynamic_cast<Arrival*>(itr.process)) delete itr.process;
    foreach_ (EntMap::value_type& itr, resource_map)
      delete itr.second;
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
    foreach_ (PQueue::value_type& itr, event_queue)
      if (dynamic_cast<Arrival*>(itr.process)) delete itr.process;
    event_queue.clear();
    event_map.clear();
    foreach_ (EntMap::value_type& itr, resource_map)
      ((Resource*)itr.second)->reset();
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
    arr_traj_stats.clear();
    arr_res_stats.clear();
    attr_stats.clear();
    res_stats.clear();
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
  std::pair<VEC<double>, VEC<std::string> > peek(int steps) {
    VEC<double> time;
    VEC<std::string> process;
    if (steps) {
      foreach_ (PQueue::value_type& itr, event_queue) {
        time.push_back(itr.time);
        process.push_back(itr.process->name);
        if (!--steps) break;
      }
    }
    return std::make_pair(time, process);
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
    long int nsteps = 0;
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
      Rcpp::warning("process " + name_prefix + " already defined");
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
   * @param   name          the name
   * @param   capacity      server capacity (-1 means infinity)
   * @param   queue_size    room in the queue (-1 means infinity)
   * @param   mon           whether this entity must be monitored
   * @param   preemptive    whether the resource is preemptive
   * @param   preempt_order fifo or lifo
   * @param   keep_queue    whether the queue size is a hard limit
   */
  bool add_resource(std::string name, int capacity, int queue_size, bool mon,
                    bool preemptive, std::string preempt_order, bool keep_queue)
  {
    if (resource_map.find(name) != resource_map.end()) {
      Rcpp::warning("resource " + name + " already defined");
      return false;
    }
    Resource* res;
    if (!preemptive) {
      res = new PriorityRes<FIFO>(this, name, mon, capacity, queue_size);
    } else {
      if (preempt_order.compare("fifo") == 0)
        res = new PreemptiveRes<FIFO>(this, name, mon, capacity, queue_size, keep_queue);
      else
        res = new PreemptiveRes<LIFO>(this, name, mon, capacity, queue_size, keep_queue);
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
      Rcpp::stop("process " + name + " already defined");
    EntMap::iterator search = resource_map.find(name);
    if (search == resource_map.end())
      Rcpp::stop("resource '" + name + "' not found (typo?)");
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
      Rcpp::stop("generator '" + name + "' not found (typo?)");
    return (Generator*)search->second;
  }

  /**
   * Get a resource by name.
   */
  Resource* get_resource(std::string name) {
    EntMap::iterator search = resource_map.find(name);
    if (search == resource_map.end())
      Rcpp::stop("resource '" + name + "' not found (typo?)");
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

  unsigned int get_batch_count() { return b_count++; }

  void broadcast(VEC<std::string> signals) {
    foreach_ (std::string signal, signals) {
      foreach_ (HandlerMap::value_type& itr, signal_map[signal])
        itr.second();
    }
  }
  void subscribe(VEC<std::string> signals, Arrival* arrival, Bind handler) {
    foreach_ (std::string signal, signals) {
      signal_map[signal][arrival] = handler;
      arrival_map[arrival].emplace(signal);
    }
  }
  void unsubscribe(VEC<std::string> signals, Arrival* arrival) {
    foreach_ (std::string signal, signals) {
      signal_map[signal].erase(arrival);
      arrival_map[arrival].erase(signal);
    }
  }

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
    arr_traj_stats.insert("name",           name);
    arr_traj_stats.insert("start_time",     start);
    arr_traj_stats.insert("end_time",       now_);
    arr_traj_stats.insert("activity_time",  activity);
    arr_traj_stats.insert("finished",       finished);
  }
  void record_release(std::string name, double start, double activity, std::string resource) {
    arr_res_stats.insert("name",            name);
    arr_res_stats.insert("start_time",      start);
    arr_res_stats.insert("end_time",        now_);
    arr_res_stats.insert("activity_time",   activity);
    arr_res_stats.insert("resource",        resource);
  }
  void record_attribute(std::string name, std::string key, double value) {
    attr_stats.insert("time",               now_);
    attr_stats.insert("name",               name);
    attr_stats.insert("key",                key);
    attr_stats.insert("value",              value);
  }
  void record_resource(std::string name, int server_count, int queue_count,
                       int capacity, int queue_size) {
    res_stats.insert("resource",            name);
    res_stats.insert("time",                now_);
    res_stats.insert("server",              server_count);
    res_stats.insert("queue",               queue_count);
    res_stats.insert("capacity",            capacity);
    res_stats.insert("queue_size",          queue_size);
  }

  /**
   * Get monitoring data.
   */
  Rcpp::List get_arr_stats(bool per_resource, bool ongoing) {
    if (!per_resource) {
      VEC<std::string> name             = arr_traj_stats.get<std::string>("name");
      VEC<double> start_time            = arr_traj_stats.get<double>("start_time");
      VEC<double> end_time              = arr_traj_stats.get<double>("end_time");
      VEC<double> activity_time         = arr_traj_stats.get<double>("activity_time");
      Rcpp::LogicalVector finished      = Rcpp::wrap(arr_traj_stats.get<bool>("finished"));
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
      return Rcpp::List::create(
          Rcpp::Named("name")           = name,
          Rcpp::Named("start_time")     = start_time,
          Rcpp::Named("end_time")       = end_time,
          Rcpp::Named("activity_time")  = activity_time,
          Rcpp::Named("finished")       = finished
      );
    } else {
      VEC<std::string> name             = arr_res_stats.get<std::string>("name");
      VEC<double> start_time            = arr_res_stats.get<double>("start_time");
      VEC<double> end_time              = arr_res_stats.get<double>("end_time");
      VEC<double> activity_time         = arr_res_stats.get<double>("activity_time");
      VEC<std::string> resource         = arr_res_stats.get<std::string>("resource");
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
      return Rcpp::List::create(
        Rcpp::Named("name")             = name,
        Rcpp::Named("start_time")       = start_time,
        Rcpp::Named("end_time")         = end_time,
        Rcpp::Named("activity_time")    = activity_time,
        Rcpp::Named("resource")         = resource
      );
    }
  }
  Rcpp::List get_attr_stats() {
    return Rcpp::List::create(
      Rcpp::Named("time")             = attr_stats.get<double>("time"),
      Rcpp::Named("name")             = attr_stats.get<std::string>("name"),
      Rcpp::Named("key")              = attr_stats.get<std::string>("key"),
      Rcpp::Named("value")            = attr_stats.get<double>("value")
    );
  }
  Rcpp::List get_res_stats() {
    return Rcpp::List::create(
      Rcpp::Named("resource")         = res_stats.get<std::string>("resource"),
      Rcpp::Named("time")             = res_stats.get<double>("time"),
      Rcpp::Named("server")           = res_stats.get<int>("server"),
      Rcpp::Named("queue")            = res_stats.get<int>("queue"),
      Rcpp::Named("capacity")         = res_stats.get<int>("capacity"),
      Rcpp::Named("queue_size")       = res_stats.get<int>("queue_size")
    );
  }
  Rcpp::List get_res_stats_counts() {
    return Rcpp::List::create(
      Rcpp::Named("resource")         = res_stats.get<std::string>("resource"),
      Rcpp::Named("time")             = res_stats.get<double>("time"),
      Rcpp::Named("server")           = res_stats.get<int>("server"),
      Rcpp::Named("queue")            = res_stats.get<int>("queue")
    );
  }
  Rcpp::List get_res_stats_limits() {
    return Rcpp::List::create(
      Rcpp::Named("resource")         = res_stats.get<std::string>("resource"),
      Rcpp::Named("time")             = res_stats.get<double>("time"),
      Rcpp::Named("server")           = res_stats.get<int>("capacity"),
      Rcpp::Named("queue")            = res_stats.get<int>("queue_size")
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
  unsigned int b_count;     /**< unnamed batch counter */
  SigMap signal_map;        /**< map of arrivals subscribed to signals */
  StatsMap arr_traj_stats;  /**< arrival statistics per trajectory */
  StatsMap arr_res_stats;   /**< arrival statistics per resource */
  StatsMap attr_stats;      /**< attribute statistics */
  StatsMap res_stats;       /**< resource statistics */
};

#endif
