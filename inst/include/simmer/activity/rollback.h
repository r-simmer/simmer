// Copyright (C) 2014-2018 Iñaki Ucar and Bart Smeets
//
// This file is part of simmer.
//
// simmer is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// simmer is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with simmer. If not, see <http://www.gnu.org/licenses/>.

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
      Activity::print(indent, verbose, brief);
      if (!cached) cached = goback();
      std::string amount = MakeString() << this->amount << " (" << cached->name << ")";
      if (check) internal::print(brief, true, ARG(amount), ARG(*check));
      else internal::print(brief, true, ARG(amount), ARG(times));
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
