#ifndef simmer__activity_resource_h
#define simmer__activity_resource_h

#include <simmer/activity.h>
#include <simmer/activity/fork.h>
#include <simmer/activity/utils/resgetter.h>
#include <simmer/activity/utils/getop.h>
#include <simmer/activity/utils/policy.h>

namespace simmer {

  /**
   * Seize a resource.
   */
  template <typename T>
  class Seize : public Fork, public ResGetter {
  public:
    CLONEABLE(Seize<T>)

    Seize(const std::string& resource, const T& amount, const VEC<bool>& cont,
          const VEC<REnv>& trj, unsigned short mask)
      : Fork("Seize", cont, trj),
        ResGetter("Seize", resource), amount(amount), mask(mask) {}

    Seize(int id, const T& amount, const VEC<bool>& cont,
          const VEC<REnv>& trj, unsigned short mask)
      : Fork("Seize", cont, trj),
        ResGetter("Seize", "[]", id), amount(amount), mask(mask) {}

    void print(unsigned int indent = 0, bool verbose = false, bool brief = false) {
      Activity::print(indent, verbose, brief);
      if (!brief) Rcpp::Rcout << LABEL2(resource, amount) << BENDL;
      else Rcpp::Rcout << BARE2(resource, amount) << SEP;
      Fork::print(indent, verbose, brief);
    }

    double run(Arrival* arrival) {
      return select_path(
        arrival, get_resource(arrival)->seize(arrival, std::abs(get<int>(amount, arrival))));
    }

  protected:
    T amount;
    unsigned short mask;

    int select_path(Arrival* arrival, int ret) {
      switch (ret) {
      case REJECT:
        if (mask & 2) {
          ret = SUCCESS;
          if (mask & 1)
            selected = heads[1];
          else
            selected = heads[0];
        } else arrival->terminate(false);
        break;
      default:
        if (mask & 1)
          selected = heads[0];
        break;
      }
      return ret;
    }
  };

  /**
   * Release a resource.
   */
  template <typename T>
  class Release : public Activity, public ResGetter {
  public:
    CLONEABLE(Release<T>)

    Release(const std::string& resource, const T& amount)
      : Activity("Release", PRIORITY_RELEASE),
        ResGetter("Release", resource), amount(amount) {}

    Release(int id, const T& amount)
      : Activity("Release", PRIORITY_RELEASE),
        ResGetter("Release", "[]", id), amount(amount) {}

    void print(unsigned int indent = 0, bool verbose = false, bool brief = false) {
      Activity::print(indent, verbose, brief);
      if (!brief) Rcpp::Rcout << LABEL2(resource, amount) << BENDL;
      else Rcpp::Rcout << BARE2(resource, amount) << ENDL;
    }

    double run(Arrival* arrival) {
      return get_resource(arrival)->release(arrival, std::abs(get<int>(amount, arrival)));
    }

  protected:
    T amount;
  };

  /**
   * Set a resource's capacity.
   */
  template <typename T>
  class SetCapacity : public Activity, public ResGetter {
  public:
    CLONEABLE(SetCapacity<T>)

    SetCapacity(const std::string& resource, const T& value, char mod='N')
      : Activity("SetCapacity"), ResGetter("SetCapacity", resource),
        value(value), mod(mod), op(get_op<double>(mod)) {}

    SetCapacity(int id, const T& value, char mod='N')
      : Activity("SetCapacity"), ResGetter("SetCapacity", "[]", id),
        value(value), mod(mod), op(get_op<double>(mod)) {}

    void print(unsigned int indent = 0, bool verbose = false, bool brief = false) {
      Activity::print(indent, verbose, brief);
      if (!brief) Rcpp::Rcout << LABEL3(resource, value, mod) << BENDL;
      else Rcpp::Rcout << BARE3(resource, value, mod) << ENDL;
    }

    double run(Arrival* arrival) {
      double ret = get<double>(value, arrival);
      double oldval = get_resource(arrival)->get_capacity();
      if (oldval < 0) oldval = R_PosInf;

      if (op) ret = op(oldval, ret);
      if (ret >= 0)
        get_resource(arrival)->set_capacity((ret == R_PosInf) ? -1 : (int) ret);

      if (arrival->is_paused())
        return ENQUEUE;
      return 0;
    }

  protected:
    T value;
    char mod;
    Fn<double(double, double)> op;
  };

  /**
   * Set a resource's queue size.
   */
  template <typename T>
  class SetQueue : public Activity, public ResGetter {
  public:
    CLONEABLE(SetQueue<T>)

    SetQueue(const std::string& resource, const T& value, char mod='N')
      : Activity("SetQueue"), ResGetter("SetQueue", resource),
        value(value), mod(mod), op(get_op<double>(mod)) {}

    SetQueue(int id, const T& value, char mod='N')
      : Activity("SetQueue"), ResGetter("SetQueue", "[]", id),
        value(value), mod(mod), op(get_op<double>(mod)) {}

    void print(unsigned int indent = 0, bool verbose = false, bool brief = false) {
      Activity::print(indent, verbose, brief);
      if (!brief) Rcpp::Rcout << LABEL3(resource, value, mod) << BENDL;
      else Rcpp::Rcout << BARE3(resource, value, mod) << ENDL;
    }

    double run(Arrival* arrival) {
      double ret = get<double>(value, arrival);
      double oldval = get_resource(arrival)->get_queue_size();
      if (oldval < 0) oldval = R_PosInf;

      if (op) ret = op(oldval, ret);
      if (ret >= 0)
        get_resource(arrival)->set_queue_size((ret == R_PosInf) ? -1 : (int) ret);

      return 0;
    }

  protected:
    T value;
    char mod;
    Fn<double(double, double)> op;
  };

  /**
   * Select a resource based on some policy.
   */
  template <typename T>
  class Select : public Activity {
  public:
    CLONEABLE(Select<T>)

    Select(const T& resources, const std::string& policy, int id)
      : Activity("Select"), resources(resources), id(id), policy(Policy(policy)) {}

    void print(unsigned int indent = 0, bool verbose = false, bool brief = false) {
      Activity::print(indent, verbose, brief);
      if (!brief) Rcpp::Rcout << LABEL2(resources, policy) << BENDL;
      else Rcpp::Rcout << BARE2(resources, policy) << ENDL;
    }

    double run(Arrival* arrival) {
      arrival->set_resource_selected(
          id, policy.dispatch(arrival->sim, get<VEC<std::string> >(resources, arrival)));
      return 0;
    }

  protected:
    T resources;
    int id;
    Policy policy;
  };

} // namespace simmer

#endif
