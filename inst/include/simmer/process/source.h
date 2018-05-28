#ifndef simmer__process_source_h
#define simmer__process_source_h

#include <simmer/process.h>
#include <simmer/process/order.h>
#include <simmer/process/arrival.h>
#include <simmer/activity.h>

namespace simmer {

  /**
   * Abstract class for source processes.
   */
  class Source : public Process {
  public:
    /**
    * Constructor.
    * @param sim             a pointer to the simulator
    * @param name_prefix     name prefix for each new arrival
    * @param mon             int that indicates whether this entity must be monitored
    * @param trj             a user-defined R trajectory
    * @param source          some source for arrivals
    * @param order           priority, preemptible, restart
    */
    Source(Simulator* sim, const std::string& name_prefix, int mon,
           const REnv& trj, const Order& order)
      : Process(sim, name_prefix, mon, PRIORITY_MIN), count(0), order(order),
        first_activity(internal::head(trj)), trj(trj) {}

    virtual void reset() { count = 0; }

    int get_n_generated() const { return count; }

    virtual void set_source(const ANY& new_source) = 0;

    void set_trajectory(const REnv& new_trj) {
      trj = new_trj;
      first_activity = internal::head(trj);
    }

  protected:
    int count;                /**< number of arrivals generated */
    Order order;
    Activity* first_activity;

    Arrival* new_arrival(double delay) {
      // format the name and create the next arrival
      std::string arr_name = MakeString() << name << count++;
      Arrival* arrival = new Arrival(sim, arr_name, is_monitored(),
                                     order, first_activity, count);

      if (sim->verbose) sim->print("source", name, "new", arr_name,
          MakeString() << (sim->now() + delay));

      return arrival;
    }

  private:
    REnv trj;
  };

} // namespace simmer

#endif
