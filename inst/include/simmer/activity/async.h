#ifndef simmer__activity_async_h
#define simmer__activity_async_h

#include <simmer/activity.h>
#include <simmer/activity/fork.h>
#include <simmer/activity/utils/macros.h>

namespace simmer {

  /**
   * Send signals.
   */
  template <typename T, typename U>
  class Send : public Activity {
  public:
    CLONEABLE(Send<T COMMA U>)

    Send(const T& signals, const U& delay)
      : Activity("Send", PRIORITY_SEND), signals(signals), delay(delay) {}

    void print(unsigned int indent = 0, bool verbose = false, bool brief = false) {
      Activity::print(indent, verbose, brief);
      if (!brief) Rcpp::Rcout << LABEL2(signals, delay) << BENDL;
      else Rcpp::Rcout << BARE2(signals, delay) << ENDL;
    }

    double run(Arrival* arrival) {
      double lag = std::abs(get<double>(delay, arrival));
      Task* task =
        new Task(arrival->sim, "Broadcast",
                 BIND(&Simulator::broadcast, arrival->sim,
                      get<VEC<std::string> >(signals, arrival)),
                 lag ? PRIORITY_MIN : PRIORITY_SIGNAL);
      task->activate(lag);
      return 0;
    }

  protected:
    T signals;
    U delay;
  };

  /**
   * Subscribe to signals and assign a handler.
   */
  template <typename T>
  class Trap : public Fork {
  public:
    CLONEABLE(Trap<T>)

    Trap(const T& signals, const VEC<REnv>& trj, bool interruptible)
      : Fork("Trap", VEC<bool>(trj.size(), false), trj, PRIORITY_TRAP),
        signals(signals), interruptible(interruptible) {}

    Trap(const Trap& o) : Fork(o), signals(o.signals), interruptible(o.interruptible) {
      pending.clear();
    }

    void print(unsigned int indent = 0, bool verbose = false, bool brief = false) {
      Activity::print(indent, verbose, brief);
      if (!brief) Rcpp::Rcout << LABEL1(signals) << BENDL;
      else Rcpp::Rcout << BARE1(signals) << SEP;
      Fork::print(indent, verbose, brief);
    }

    double run(Arrival* arrival) {
      if (!interruptible && pending.find(arrival) != pending.end()) {
        arrival->sim->subscribe(arrival);
        arrival->set_activity(pending[arrival]);
        pending.erase(arrival);
        arrival->activate();
        return REJECT;
      }
      arrival->sim->subscribe(get<VEC<std::string> >(signals, arrival), arrival,
                              BIND(&Trap::launch_handler, this, arrival));
      return 0;
    }

  protected:
    T signals;
    bool interruptible;
    UMAP<Arrival*, Activity*> pending;

    void launch_handler(Arrival* arrival) {
      if (!arrival->sim->is_scheduled(arrival))
        return;
      arrival->stop();
      if (heads.size()) {
        if (!interruptible) {
          arrival->sim->unsubscribe(arrival);
          pending[arrival] = arrival->get_activity();
          tails[0]->set_next(this);
        } else {
          tails[0]->set_next(arrival->get_activity());
        }
        arrival->set_activity(heads[0]);
      }
      arrival->activate();
    }
  };

  /**
   * Unsubscribe to signals.
   */
  template <typename T>
  class UnTrap : public Activity {
  public:
    CLONEABLE(UnTrap<T>)

    UnTrap(const T& signals) : Activity("UnTrap", PRIORITY_TRAP), signals(signals) {}

    void print(unsigned int indent = 0, bool verbose = false, bool brief = false) {
      Activity::print(indent, verbose, brief);
      if (!brief) Rcpp::Rcout << LABEL1(signals) << BENDL;
      else Rcpp::Rcout << BARE1(signals) << ENDL;
    }

    double run(Arrival* arrival) {
      arrival->sim->unsubscribe(get<VEC<std::string> >(signals, arrival), arrival);
      return 0;
    }

  protected:
    T signals;
  };

  /**
   * Block until a signal is received.
   */
  class Wait : public Activity {
  public:
    CLONEABLE(Wait)

    Wait() : Activity("Wait") {}

    void print(unsigned int indent = 0, bool verbose = false, bool brief = false) {
      Activity::print(indent, verbose, brief);
      if (!brief) Rcpp::Rcout << BENDL;
      else Rcpp::Rcout << ENDL;
    }

    double run(Arrival* arrival) { return BLOCK; }
  };

} // namespace simmer

#endif
