#ifndef SOURCE_H
#define SOURCE_H

#include "process.h"
#include "order.h"

// forward declarations
class Activity;
class Arrival;

Activity* trj_head(const REnv& trj);

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
      first_activity(trj_head(trj)), trj(trj) {}

  virtual void reset() { count = 0; }

  int get_n_generated() const { return count; }

  virtual void set_source(const ANY& new_source) = 0;

  void set_trajectory(const REnv& new_trj) {
    trj = new_trj;
    first_activity = trj_head(trj);
  }

protected:
  int count;                /**< number of arrivals generated */
  Order order;
  Activity* first_activity;

  Arrival* new_arrival(double delay);

private:
  REnv trj;
};

#endif
