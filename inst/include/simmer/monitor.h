// Copyright (C) 2018 Iñaki Ucar
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

#ifndef simmer__monitor_h
#define simmer__monitor_h

#include <simmer/common.h>

namespace simmer {

  class Monitor {
  public:
    Monitor() {
      ends_h = vec_of<std::string>("name")("start_time")("end_time")("activity_time")("finished");
      releases_h = vec_of<std::string>("name")("start_time")("end_time")("activity_time")("resource");
      attributes_h = vec_of<std::string>("time")("name")("key")("value");
      resources_h = vec_of<std::string>("resource")("time")("server")("queue")("capacity")("queue_size");
    }

    virtual ~Monitor() {}
    virtual void clear() = 0;
    virtual void flush() {}

    virtual void record_end(const std::string& name, double start, double end,
                            double activity, bool finished) = 0;
    virtual void record_release(const std::string& name, double start, double end,
                                double activity, const std::string& resource) = 0;
    virtual void record_attribute(double time, const std::string& name,
                                  const std::string& key, double value) = 0;
    virtual void record_resource(const std::string& name, double time, int server_count,
                                 int queue_count, int capacity, int queue_size) = 0;

  protected:
    VEC<std::string> ends_h;
    VEC<std::string> releases_h;
    VEC<std::string> attributes_h;
    VEC<std::string> resources_h;
  };

} // namespace simmer

#endif
