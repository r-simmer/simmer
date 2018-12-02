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

#ifndef simmer__activity_debug_h
#define simmer__activity_debug_h

#include <simmer/activity.h>
#include <simmer/process/arrival.h>

namespace simmer {

  /**
   * Print a message.
   */
  template <typename T>
  class Log : public Activity {
  public:
    CLONEABLE(Log<T>)

    Log(const T& message, int level)
      : Activity("Log"), message(message), level(level) {}

    void print(unsigned int indent = 0, bool verbose = false, bool brief = false) {
      Activity::print(indent, verbose, brief);
      std::ostringstream ss; ss << message;
      std::string m = ss.str();
      if (m.size() > 10) m = m.substr(0, 10) + "...";
      internal::print(brief, true, "message: ", m, ARG(level));
    }

    double run(Arrival* arrival) {
      int log_level = arrival->sim->log_level;
      if (log_level < 0 || (level >= 0 && log_level >= level))
        Rcpp::Rcout << arrival->sim->now() << ": " << arrival->name << ": " <<
          get<std::string>(message, arrival) << std::endl;
      return 0;
    }

  protected:
    T message;
    int level;
  };

  /**
   * Set a breakpoint based on some condition.
   */
  template <typename T>
  class StopIf : public Activity {
  public:
    CLONEABLE(StopIf<T>)

    StopIf(const T& condition)
      : Activity("StopIf"), condition(condition) {}

    void print(unsigned int indent = 0, bool verbose = false, bool brief = false) {
      Activity::print(indent, verbose, brief);
      internal::print(brief, true, ARG(condition));
    }

    double run(Arrival* arrival) {
      if (get<bool>(condition, arrival))
        arrival->sim->request_stop();
      return 0;
    }

  protected:
    T condition;
  };

} // namespace simmer

#endif
