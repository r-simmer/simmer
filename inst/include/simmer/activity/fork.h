// Copyright (C) 2015-2016 Bart Smeets and Iñaki Ucar
// Copyright (C) 2016-2018,2020 Iñaki Ucar
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

#ifndef simmer__activity_fork_h
#define simmer__activity_fork_h

#include <simmer/activity.h>

namespace simmer {

  // abstract class for multipath activities
  class Fork : public virtual Activity {
  public:
    Fork(const VEC<bool>& cont, const VEC<REnv>& trj)
      : cont(cont), trj(trj), path(-1)
    {
      foreach_ (const VEC<REnv>::value_type& itr, trj) {
        Activity* head = internal::head(itr);
        if (head) head->set_prev(this);
        heads.push_back(head);
        tails.push_back(internal::tail(itr));
        count += internal::get_n_activities(itr);
      }
    }

    Fork(const Fork& o) : cont(o.cont), trj(o.trj), path(-1) {
      heads.clear();
      tails.clear();
      foreach_ (VEC<REnv>::value_type& itr, trj) {
        itr = internal::clone(itr);
        Activity* head = internal::head(itr);
        if (head) head->set_prev(this);
        heads.push_back(head);
        tails.push_back(internal::tail(itr));
      }
    }

    void print(unsigned int indent = 0, bool verbose = false, bool brief = false) {
      indent += 2;
      if (!brief) {
        if (indent > 10) return; // max 6 levels
        for (unsigned int i = 0; i < trj.size(); i++) {
          Rcpp::Rcout << IND(indent) <<
            "Fork " << i+1 << (cont[i] ? ", continue," : ", stop,");
          internal::print(trj[i], indent, verbose);
        }
      } else Rcpp::Rcout << trj.size() << " paths" << std::endl;
    }

    void set_next(Activity* activity) {
      Activity::set_next(activity);
      for (unsigned int i = 0; i < tails.size(); i++) {
        if (cont[i] && tails[i]) tails[i]->set_next(activity);
      }
    }

    Activity* get_next() {
      if (path >= 0) {
        int selected = path;
        path = -1;
        if (heads[selected] || !cont[selected])
          return heads[selected];
      }
      return Activity::get_next();
    }

  protected:
    VEC<bool> cont;
    VEC<REnv> trj;
    int path;
    VEC<Activity*> heads;
    VEC<Activity*> tails;
  };

} // namespace simmer

#endif
