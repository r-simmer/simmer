#ifndef simmer__process_order_h
#define simmer__process_order_h

#include <simmer/common.h>

struct Order {
public:
  Order(int priority=0, int preemptible=0, bool restart=false)
    : preemptible(preemptible)
  {
    set_priority(priority);
    set_preemptible(preemptible);
    set_restart(restart);
  }

  void set_priority(int value) {
    priority = value;
    if (preemptible < priority)
      preemptible = priority;
  }
  int get_priority() const { return priority; }
  void set_preemptible(int value) {
    if (value < priority) {
      Rcpp::warning("`preemptible` level cannot be < `priority`, `preemptible` set to %d", priority);
      value = priority;
    }
    preemptible = value;
  }
  int get_preemptible() const { return preemptible; }
  void set_restart(bool value) { restart = value; }
  bool get_restart() const { return restart; }

private:
  int priority;       /**< arrival priority */
  int preemptible;    /**< maximum priority that cannot cause preemption (>=priority) */
  bool restart;       /**< whether activity must be restarted after preemption */
};

#endif
