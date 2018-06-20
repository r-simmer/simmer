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

#ifndef simmer__monitor_csv_h
#define simmer__monitor_csv_h

#include <simmer/monitor.h>
#include <fstream>

namespace simmer {

  namespace internal {

    class CsvWriter : public std::ofstream {
    public:
      void open(const std::string& path, VEC<std::string> header, char sep=',') {
        std::ofstream::open(path.c_str());
        setf(std::ios_base::fixed);
        precision(9);
        i = 0;
        n_cols = (int) header.size();
        this->sep = sep;
        foreach_ (const std::string& name, header)
          *this << name;
        flush();
      }

      template <typename T>
      friend CsvWriter& operator<<(CsvWriter& ofs, const T& elem) {
        std::ofstream& ofsp = (std::ofstream&) ofs;
        if (ofs.i++ > 0)
          ofsp << ofs.sep;
        ofsp << elem;
        if (ofs.i == ofs.n_cols) {
          ofsp << '\n';
          ofs.i = 0;
        }
        return ofs;
      }

    private:
      int i;
      int n_cols;
      char sep;
    };

  } // namespace internal

  class CsvMonitor : public Monitor {
  public:
    CsvMonitor(const std::string& ends_path, const std::string& releases_path,
               const std::string& attributes_path, const std::string& resources_path,
               char sep=',')
      : Monitor(), ends_path(ends_path), releases_path(releases_path),
        attributes_path(attributes_path), resources_path(resources_path), sep(sep)
    {
      ends.open(ends_path, ends_h, sep);
      releases.open(releases_path, releases_h, sep);
      attributes.open(attributes_path, attributes_h, sep);
      resources.open(resources_path, resources_h, sep);
    }

    void clear() {
      ends.close();
      releases.close();
      attributes.close();
      resources.close();
      ends.open(ends_path, ends_h, sep);
      releases.open(releases_path, releases_h, sep);
      attributes.open(attributes_path, attributes_h, sep);
      resources.open(resources_path, resources_h, sep);
    }

    void flush() {
      ends.flush();
      releases.flush();
      attributes.flush();
      resources.flush();
    }

    void record_end(const std::string& name, double start, double end,
                    double activity, bool finished)
    {
      ends << name << start << end << activity << finished;
    }

    void record_release(const std::string& name, double start, double end,
                        double activity, const std::string& resource)
    {
      releases << name << start << end << activity << resource;
    }

    void record_attribute(double time, const std::string& name,
                          const std::string& key, double value)
    {
      attributes << time << name << key << value;
    }

    void record_resource(const std::string& name, double time, int server_count,
                         int queue_count, int capacity, int queue_size)
    {
      resources << name << time << server_count << queue_count << capacity << queue_size;
    }

  private:
    std::string ends_path;
    std::string releases_path;
    std::string attributes_path;
    std::string resources_path;
    char sep;
    internal::CsvWriter ends;
    internal::CsvWriter releases;
    internal::CsvWriter attributes;
    internal::CsvWriter resources;
  };

} // namespace simmer

#endif
