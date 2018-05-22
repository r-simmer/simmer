#ifndef simmer__activity_rollback_h
#define simmer__activity_rollback_h

#include <simmer/activity.h>

namespace simmer {

  /**
   * Rollback to a previous activity.
   */
  class Rollback : public Activity {
  public:
    CLONEABLE(Rollback)

    Rollback(int amount, int times, const OPT<RFn>& check = NONE)
      : Activity("Rollback"), amount(std::abs(amount)),
        times(times), check(check), cached(NULL), selected(NULL) {}

    Rollback(const Rollback& o)
      : Activity(o), amount(o.amount), times(o.times), check(o.check),
        cached(NULL), selected(NULL) { pending.clear(); }

    void print(unsigned int indent = 0, bool verbose = false, bool brief = false) {
      if (!cached) cached = goback();
      Activity::print(indent, verbose, brief);
      if (!brief) {
        Rcpp::Rcout << LABEL1(amount) << " (" << cached->name << "), ";
        if (check) Rcpp::Rcout << LABEL1(*check) << BENDL;
        else Rcpp::Rcout << LABEL1(times) << BENDL;
      } else Rcpp::Rcout << BARE1(cached->name) << ENDL;
    }

    double run(Arrival* arrival) {
      if (check) {
        if (!get<bool>(*check, arrival))
          return 0;
      } else if (times >= 0) {
        if (pending.find(arrival) == pending.end())
          pending[arrival] = times;
        if (!pending[arrival]) {
          pending.erase(arrival);
          return 0;
        }
        pending[arrival]--;
      }
      if (!cached) cached = goback();
      selected = cached;
      return 0;
    }

    Activity* get_next() {
      if (selected) {
        Activity* aux = selected;
        selected = NULL;
        return aux;
      }
      return Activity::get_next();
    }

  protected:
    int amount;
    int times;
    OPT<RFn> check;
    Activity* cached, *selected;
    UMAP<Arrival*, int> pending;

    Activity* goback() {
      int n = amount;
      Activity* ptr = this;
      while (ptr->get_prev() && n--)
        ptr = ptr->get_prev();
      return ptr;
    }
  };

} // namespace simmer

#endif
