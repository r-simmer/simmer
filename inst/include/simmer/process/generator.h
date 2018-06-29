// Copyright (C) 2015-2016 Bart Smeets and Iñaki Ucar
// Copyright (C) 2016-2018 Iñaki Ucar
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

#ifndef simmer__process_generator_h
#define simmer__process_generator_h

#include <simmer/process/source.h>
#include <simmer/activity.h>

namespace simmer {

  /**
   * Generation process.
   */
  class Generator : public Source {
  public:
    Generator(Simulator* sim, const std::string& name_prefix, int mon,
              const REnv& trj, const RFn& dist, const Order& order)
      : Source(sim, name_prefix, mon, trj, order), source(dist) {}

    void reset() {
      Source::reset();
      RFn reset_fun(source.attr("reset"));
      reset_fun();
    }

    void set_source(const ANY& new_source) {
      if (new_source.type() != typeid(RFn))
        Rcpp::stop("function required");
      source = boost::any_cast<RFn>(new_source);
    }

    void run() {
      // get the delay for the next (n) arrival(s)
      RNum delays = source();
      size_t n = delays.size();
      double delay = 0;

      for (size_t i = 0; i < n; ++i) {
        if (delays[i] < 0)
          return;
        delay += delays[i];

        // schedule the arrival
        sim->schedule(delay, new_arrival(delay),
                      first_activity && first_activity->priority ?
                        first_activity->priority : count);
      }
      // schedule the generator
      sim->schedule(delay, this, priority);
    }

  private:
    RFn source;
  };

} // namespace simmer

#endif
