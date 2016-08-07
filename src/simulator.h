#ifndef SIMULATOR_H
#define SIMULATOR_H

#include "simmer.h"
#include "stats.h"
#include "process.h"
#include "resource.h"

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
  
public:
  std::string name;
  bool verbose;
  
  /**
   * Constructor.
   * @param name    simulator name
   * @param verbose verbose flag
   */
  Simulator(std::string name, bool verbose): name(name), verbose(verbose), now_(0) {}
  
  ~Simulator() {
    foreach_ (PQueue::value_type& itr, event_queue)
      if (dynamic_cast<Arrival*>(itr.process)) delete itr.process;
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
      ((Process*)itr.second)->run();
    }
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
    if (steps) { foreach_ (PQueue::value_type& itr, event_queue) {
      time.push_back(itr.time);
      process.push_back(itr.process->name);
      if (!--steps) break;
    }}
    return std::make_pair(time, process);
  }
  
  /**
   * Process the next event. Only one step, a giant leap for mankind.
   */
  bool step() {
    if (event_queue.empty()) return 0;
    PQueue::iterator ev = event_queue.begin();
    event_map.erase(ev->process);
    now_ = ev->time;
    ev->process->run();
    event_queue.erase(ev);
    return 1;
    // ... and that's it! :D
  }
  
  /**
   * Executes steps until the given criterion is met.
   * @param   until   time of ending
   */
  void run(double until) {
    long int nsteps = 0;
    while ((now_ < until || until < 0) && step())
      if (++nsteps % 100000 == 0)
        Rcpp::checkUserInterrupt();
  }
  
  /**
   * Add a generator of arrivals to the simulator.
   * @param   name_prefix     prefix for the arrival names
   * @param   first_activity  the first activity of a user-defined R trajectory
   * @param   dis             an user-defined R function that provides random numbers
   * @param   mon             monitoring level
   * @param   priority        arrival priority
   * @param   preemptible     maximum priority that cannot cause preemption (>=priority)
   * @param   restart         whether activity must be restarted after preemption
   */
  bool add_generator(std::string name_prefix, Activity* first_activity, Rcpp::Function dist, 
                     int mon, int priority, int preemptible, bool restart) {
    if (process_map.find(name_prefix) == process_map.end()) {
      Generator* gen = new Generator(this, name_prefix, mon, first_activity, dist,
                                     Order(priority, preemptible, restart));
      process_map[name_prefix] = gen;
      gen->run();
      return TRUE;
    }
    Rcpp::warning("process " + name_prefix + " already defined");
    return FALSE;
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
                    bool preemptive, std::string preempt_order, bool keep_queue) {
    if (resource_map.find(name) == resource_map.end()) {
      Resource* res;
      if (!preemptive)
        res = new PriorityRes<FIFO>(this, name, mon, capacity, queue_size);
      else {
        if (preempt_order.compare("fifo") == 0)
          res = new PreemptiveRes<FIFO>(this, name, mon, capacity, queue_size, keep_queue);
        else res = new PreemptiveRes<LIFO>(this, name, mon, capacity, queue_size, keep_queue);
      }
      resource_map[name] = res;
      return TRUE;
    }
    Rcpp::warning("resource " + name + " already defined");
    return FALSE;
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
                            VEC<double> duration, VEC<int> value, int period) {
    if (process_map.find(name) == process_map.end()) {
      EntMap::iterator search = resource_map.find(name);
      if (search == resource_map.end())
        Rcpp::stop("resource '" + name + "' not found (typo?)");
      Resource* res = (Resource*)search->second;
      
      Manager* manager;
      if (param.compare("capacity") == 0)
        manager = new Manager(this, name, param, duration, value, period,
                              boost::bind(&Resource::set_capacity, res, _1));
      else manager = new Manager(this, name, param, duration, value, period,
                                 boost::bind(&Resource::set_queue_size, res, _1));
      process_map[name + "_" + param] = manager;
      manager->run();
      return TRUE;
    }
    Rcpp::warning("process " + name + " already defined");
    return FALSE;
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
  Rcpp::List get_arr_traj_stats() {
    return Rcpp::List::create(
        Rcpp::Named("name")           = arr_traj_stats.get<std::string>("name"),
        Rcpp::Named("start_time")     = arr_traj_stats.get<double>("start_time"),
        Rcpp::Named("end_time")       = arr_traj_stats.get<double>("end_time"),
        Rcpp::Named("activity_time")  = arr_traj_stats.get<double>("activity_time"),
        Rcpp::Named("finished")       = arr_traj_stats.get<bool>("finished")
    );
  }
  Rcpp::List get_arr_res_stats() {
    return Rcpp::List::create(
      Rcpp::Named("name")             = arr_res_stats.get<std::string>("name"),
      Rcpp::Named("start_time")       = arr_res_stats.get<double>("start_time"),
      Rcpp::Named("end_time")         = arr_res_stats.get<double>("end_time"),
      Rcpp::Named("activity_time")    = arr_res_stats.get<double>("activity_time"),
      Rcpp::Named("resource")         = arr_res_stats.get<std::string>("resource")
    );
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
  StatsMap arr_traj_stats;  /**< arrival statistics per trajectory */
  StatsMap arr_res_stats;   /**< arrival statistics per resource */
  StatsMap attr_stats;      /**< attribute statistics */
  StatsMap res_stats;       /**< resource statistics */
};

#endif
