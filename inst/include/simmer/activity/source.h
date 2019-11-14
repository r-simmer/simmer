// Copyright (C) 2016-2019 Iñaki Ucar
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
#include <simmer/process/source.h>

namespace simmer {

  /**
   * Activate sources.
   */
  template <typename T>
  class Activate : public Activity {
  public:
    CLONEABLE(Activate<T>)

    Activate(const T& sources)
      : Activity("Activate"), sources(sources) {}

    void print(unsigned int indent = 0, bool verbose = false, bool brief = false) {
      Activity::print(indent, verbose, brief);
      internal::print(brief, true, ARG(sources));
    }

    double run(Arrival* arrival) {
      VEC<std::string> src = get<VEC<std::string> >(sources, arrival);
      for (unsigned int i = 0; i < src.size(); i++)
        arrival->sim->get_source(src[i])->activate();
      return 0;
    }

  protected:
    T sources;
  };

  /**
   * Deactivate sources.
   */
  template <typename T>
  class Deactivate : public Activity {
  public:
    CLONEABLE(Deactivate<T>)

    Deactivate(const T& sources)
      : Activity("Deactivate"), sources(sources) {}

    void print(unsigned int indent = 0, bool verbose = false, bool brief = false) {
      Activity::print(indent, verbose, brief);
      internal::print(brief, true, ARG(sources));
    }

    double run(Arrival* arrival) {
      VEC<std::string> src = get<VEC<std::string> >(sources, arrival);
      for (unsigned int i = 0; i < src.size(); i++)
        arrival->sim->get_source(src[i])->deactivate();
      return 0;
    }

  protected:
    T sources;
  };

  /**
   * Set sources' source.
   */
  template <typename T, typename U>
  class SetSource : public Activity {
  public:
    CLONEABLE(SetSource<T COMMA U>)

    SetSource(const T& sources, const U& object)
      : Activity("SetSource"), sources(sources), object(object) {}

    void print(unsigned int indent = 0, bool verbose = false, bool brief = false) {
      Activity::print(indent, verbose, brief);
      internal::print(brief, true, ARG(sources), ARG(object));
    }

    double run(Arrival* arrival) {
      VEC<std::string> src = get<VEC<std::string> >(sources, arrival);
      for (unsigned int i = 0; i < src.size(); i++)
        arrival->sim->get_source(src[i])->set_source(object);
      return 0;
    }

  protected:
    T sources;
    U object;
  };

  /**
   * Set sources' trajectory.
   */
  template <typename T>
  class SetTraj : public Activity {
  public:
    CLONEABLE(SetTraj<T>)

    SetTraj(const T& sources, const REnv& trajectory)
      : Activity("SetTraj"), sources(sources), trajectory(trajectory) {}

    void print(unsigned int indent = 0, bool verbose = false, bool brief = false) {
      Activity::print(indent, verbose, brief);
      internal::print(brief, true, ARG(sources), ARG(trajectory));
    }

    double run(Arrival* arrival) {
      VEC<std::string> src = get<VEC<std::string> >(sources, arrival);
      for (unsigned int i = 0; i < src.size(); i++)
        arrival->sim->get_source(src[i])->set_trajectory(trajectory);
      return 0;
    }

  protected:
    T sources;
    REnv trajectory;
  };

} // namespace simmer

#endif
