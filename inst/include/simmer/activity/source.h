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

#ifndef simmer__activity_source_h
#define simmer__activity_source_h

#include <simmer/activity.h>

namespace simmer {

  /**
   * Activate a source.
   */
  template <typename T>
  class Activate : public Activity {
  public:
    CLONEABLE(Activate<T>)

    Activate(const T& source)
      : Activity("Activate"), source(source) {}

    void print(unsigned int indent = 0, bool verbose = false, bool brief = false) {
      Activity::print(indent, verbose, brief);
      internal::print(brief, true, ARG(source));
    }

    double run(Arrival* arrival) {
      arrival->sim->get_source(get<std::string>(source, arrival))->activate();
      return 0;
    }

  protected:
    T source;
  };

  /**
   * Deactivate a source.
   */
  template <typename T>
  class Deactivate : public Activity {
  public:
    CLONEABLE(Deactivate<T>)

    Deactivate(const T& source)
      : Activity("Deactivate"), source(source) {}

    void print(unsigned int indent = 0, bool verbose = false, bool brief = false) {
      Activity::print(indent, verbose, brief);
      internal::print(brief, true, ARG(source));
    }

    double run(Arrival* arrival) {
      arrival->sim->get_source(get<std::string>(source, arrival))->deactivate();
      return 0;
    }

  protected:
    T source;
  };

  /**
   * Set a source's source.
   */
  template <typename T, typename U>
  class SetSource : public Activity {
  public:
    CLONEABLE(SetSource<T COMMA U>)

    SetSource(const T& source, const U& object)
      : Activity("SetSource"), source(source), object(object) {}

    void print(unsigned int indent = 0, bool verbose = false, bool brief = false) {
      Activity::print(indent, verbose, brief);
      internal::print(brief, true, ARG(source), ARG(object));
    }

    double run(Arrival* arrival) {
      arrival->sim->
        get_source(get<std::string>(source, arrival))->set_source(object);
      return 0;
    }

  protected:
    T source;
    U object;
  };

  /**
   * Set a source's trajectory.
   */
  template <typename T>
  class SetTraj : public Activity {
  public:
    CLONEABLE(SetTraj<T>)

    SetTraj(const T& source, const REnv& trajectory)
      : Activity("SetTraj"), source(source), trajectory(trajectory) {}

    void print(unsigned int indent = 0, bool verbose = false, bool brief = false) {
      Activity::print(indent, verbose, brief);
      internal::print(brief, true, ARG(source), ARG(trajectory));
    }

    double run(Arrival* arrival) {
      arrival->sim->
        get_source(get<std::string>(source, arrival))->set_trajectory(trajectory);
      return 0;
    }

  protected:
    T source;
    REnv trajectory;
  };

} // namespace simmer

#endif
