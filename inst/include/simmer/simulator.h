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

    /**
     * Constructor.
     * @param name    simulator name
     * @param verbose verbose flag
     * @param mon     monitoring object
     */
    Simulator(const std::string& name, bool verbose, Monitor* mon)
      : name(name), verbose(verbose), mon(mon), now_(0), process_(NULL), b_count(0) {}

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

    void print(const std::string& e_type,      const std::string& e_name,
               const std::string& a_type = "", const std::string& a_name = "",
               const std::string& trail = "",  bool flush=true) const
    {
      Rcpp::Rcout <<
        FMT(10, right) << now_ << " |" <<
        FMT(12, right) << e_type + ": " << FMT(15, left) << e_name << "|" <<
        FMT(12, right) << a_type + ": " << FMT(15, left) << a_name << "| " << trail;
      if (flush) Rcpp::Rcout << std::endl;
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
                       int mon, int priority, int preemptible, bool restart);

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
                       const OPT<std::string>& restart);

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
                      bool preemptive, const std::string& preempt_order, bool queue_size_strict);

    /**
     * Add a process that manages the capacity or the queue_size of a resource
     * given a scheduling.
     * @param   name      the resource name
     * @param   param     "capacity" or "queue_size"
     * @param   duration  vector of durations until the next value change
     * @param   value     vector of values
     */
    bool add_resource_manager(const std::string& name, const std::string& param,
                              const VEC<double>& duration, const VEC<int>& value, int period);

    Source* get_source(const std::string& name) const;
    Resource* get_resource(const std::string& name) const;
    Arrival* get_running_arrival() const;
    void record_ongoing(bool per_resource) const;

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

    void broadcast(const VEC<std::string>& signals);
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

    /**
     * Process the next event. Only one step, a giant leap for mankind.
     */
    bool _step(double until = -1);
  };

} // namespace simmer

#endif
