#ifndef simmer__process_datasrc_h
#define simmer__process_datasrc_h

#include <simmer/process/source.h>
#include <simmer/process/arrival.h>
#include <simmer/activity.h>

namespace simmer {

  class DataSrc : public Source {
  public:
    DataSrc(Simulator* sim, const std::string& name_prefix, int mon,
            const REnv& trj, RData data, int batch, const std::string& time,
            const VEC<std::string>& attrs, const OPT<std::string>& priority,
            const OPT<std::string>& preemptible, const OPT<std::string>& restart)
      : Source(sim, name_prefix, mon, trj, Order()), source(data), batch(batch),
        col_time(time), col_attrs(attrs), col_priority(priority),
        col_preemptible(preemptible), col_restart(restart) {}

    void run() {
      double delay = 0;
      RNum col, time = source[col_time];
      int i = 0;

      while (i++ != batch) {
        if (time.size() <= count)
          return;
        delay += time[count];

        Arrival* arrival = new_arrival(delay);

        for (size_t j = 0; j < col_attrs.size(); ++j) {
          col = source[col_attrs[j]];
          arrival->set_attribute(col_attrs[j], col[count-1]);
        }

        if (col_priority) {
          col = source[*col_priority];
          arrival->order.set_priority(col[count-1]);
        }
        if (col_preemptible) {
          col = source[*col_preemptible];
          arrival->order.set_preemptible(col[count-1]);
        }
        if (col_restart) {
          col = source[*col_restart];
          arrival->order.set_restart(col[count-1]);
        }

        // schedule the arrival
        sim->schedule(delay, arrival,
                      first_activity->priority ? first_activity->priority : count);
      }
      // schedule the generator
      sim->schedule(delay, this, priority);
    }

    void set_source(const ANY& new_source) {
      if (new_source.type() != typeid(RData))
        Rcpp::stop("source '%s': requires a data frame", name);
      source = boost::any_cast<RData>(new_source);

      VEC<std::string> n = source.names();
      if (std::find(n.begin(), n.end(), col_time) == n.end())
        Rcpp::stop("source '%s': column '%s' not present", name, col_time);
      if (col_priority && std::find(n.begin(), n.end(), *col_priority) == n.end())
        Rcpp::stop("source '%s': column '%s' not present", name, *col_priority);
      if (col_preemptible && std::find(n.begin(), n.end(), *col_preemptible) == n.end())
        Rcpp::stop("source '%s': column '%s' not present", name, *col_preemptible);
      if (col_restart && std::find(n.begin(), n.end(), *col_restart) == n.end())
        Rcpp::stop("source '%s': column '%s' not present", name, *col_restart);
      for (size_t i = 0; i < col_attrs.size(); ++i) {
        if (std::find(n.begin(), n.end(), col_attrs[i]) == n.end())
          Rcpp::stop("source '%s': column '%s' not present", name, col_attrs[i]);
      }
    }

  private:
    RData source;
    int batch;
    std::string col_time;
    VEC<std::string> col_attrs;
    OPT<std::string> col_priority;
    OPT<std::string> col_preemptible;
    OPT<std::string> col_restart;
  };

} // namespace simmer

#endif
