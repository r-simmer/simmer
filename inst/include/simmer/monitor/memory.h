// Copyright (C) 2015-2018 Iñaki Ucar
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

#ifndef simmer__monitor_memory_h
#define simmer__monitor_memory_h

#include <simmer/monitor.h>
#include <boost/variant.hpp>

namespace simmer {

  namespace internal {

    class MonitorMap {
      typedef boost::variant< VEC<bool>, VEC<int>, VEC<double>, VEC<std::string> > _vec;
      typedef UMAP<std::string, _vec> _map;

    public:
      template <typename T>
      VEC<T> get(const std::string& key) const {
        _map::const_iterator search = map.find(key);
        if (search != map.end())
          return boost::get< VEC<T> >(search->second);
        return VEC<T>();
      }

      template <typename T>
      void push_back(const std::string& key, const T& value) {
        if (map.find(key) == map.end())
          map[key] = VEC<T>();
        boost::get< VEC<T> >(map[key]).push_back(value);
      }

      void clear() { map.clear(); }

    private:
      _map map;
    };

  } // namespace internal

  class MemMonitor : public Monitor {
  public:
    void clear() {
      ends.clear();
      releases.clear();
      attributes.clear();
      resources.clear();
    }

    void record_end(const std::string& name, double start, double end,
                    double activity, bool finished)
    {
      ends.push_back(ends_h[0], name);
      ends.push_back(ends_h[1], start);
      ends.push_back(ends_h[2], end);
      ends.push_back(ends_h[3], activity);
      ends.push_back(ends_h[4], finished);
    }

    void record_release(const std::string& name, double start, double end,
                        double activity, const std::string& resource)
    {
      releases.push_back(releases_h[0], name);
      releases.push_back(releases_h[1], start);
      releases.push_back(releases_h[2], end);
      releases.push_back(releases_h[3], activity);
      releases.push_back(releases_h[4], resource);
    }

    void record_attribute(double time, const std::string& name,
                          const std::string& key, double value)
    {
      attributes.push_back(attributes_h[0], time);
      attributes.push_back(attributes_h[1], name);
      attributes.push_back(attributes_h[2], key);
      attributes.push_back(attributes_h[3], value);
    }

    void record_resource(const std::string& name, double time, int server_count,
                         int queue_count, int capacity, int queue_size)
    {
      resources.push_back(resources_h[0], name);
      resources.push_back(resources_h[1], time);
      resources.push_back(resources_h[2], server_count);
      resources.push_back(resources_h[3], queue_count);
      resources.push_back(resources_h[4], capacity);
      resources.push_back(resources_h[5], queue_size);
    }

    RData get_arrivals(bool per_resource) const {
      if (!per_resource) return RData::create(
        Rcpp::Named(ends_h[0]) = ends.get<std::string>(ends_h[0]),
        Rcpp::Named(ends_h[1]) = ends.get<double>(ends_h[1]),
        Rcpp::Named(ends_h[2]) = ends.get<double>(ends_h[2]),
        Rcpp::Named(ends_h[3]) = ends.get<double>(ends_h[3]),
        Rcpp::Named(ends_h[4]) = ends.get<bool>(ends_h[4]),
        Rcpp::Named("stringsAsFactors") = false
      );
      return RData::create(
        Rcpp::Named(releases_h[0]) = releases.get<std::string>(releases_h[0]),
        Rcpp::Named(releases_h[1]) = releases.get<double>(releases_h[1]),
        Rcpp::Named(releases_h[2]) = releases.get<double>(releases_h[2]),
        Rcpp::Named(releases_h[3]) = releases.get<double>(releases_h[3]),
        Rcpp::Named(releases_h[4]) = releases.get<std::string>(releases_h[4]),
        Rcpp::Named("stringsAsFactors") = false
      );
    }

    RData get_attributes() const {
      return RData::create(
        Rcpp::Named(attributes_h[0]) = attributes.get<double>(attributes_h[0]),
        Rcpp::Named(attributes_h[1]) = attributes.get<std::string>(attributes_h[1]),
        Rcpp::Named(attributes_h[2]) = attributes.get<std::string>(attributes_h[2]),
        Rcpp::Named(attributes_h[3]) = attributes.get<double>(attributes_h[3]),
        Rcpp::Named("stringsAsFactors") = false
      );
    }

    RData get_resources() const {
      return RData::create(
        Rcpp::Named(resources_h[0]) = resources.get<std::string>(resources_h[0]),
        Rcpp::Named(resources_h[1]) = resources.get<double>(resources_h[1]),
        Rcpp::Named(resources_h[2]) = resources.get<int>(resources_h[2]),
        Rcpp::Named(resources_h[3]) = resources.get<int>(resources_h[3]),
        Rcpp::Named(resources_h[4]) = resources.get<int>(resources_h[4]),
        Rcpp::Named(resources_h[5]) = resources.get<int>(resources_h[5]),
        Rcpp::Named("stringsAsFactors") = false
      );
    }

  private:
    internal::MonitorMap ends;        /**< arrival statistics per trajectory */
    internal::MonitorMap releases;    /**< arrival statistics per resource */
    internal::MonitorMap attributes;  /**< attribute statistics */
    internal::MonitorMap resources;   /**< resource statistics */
  };

} // namespace simmer

#endif
