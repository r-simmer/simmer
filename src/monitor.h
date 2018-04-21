#ifndef MONITOR_H
#define MONITOR_H

#include "simmer.h"

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
  MonitorMap ends;        /**< arrival statistics per trajectory */
  MonitorMap releases;    /**< arrival statistics per resource */
  MonitorMap attributes;  /**< attribute statistics */
  MonitorMap resources;   /**< resource statistics */
};

class CsvWriter : public std::ofstream {
public:
  void open(const std::string& path, VEC<std::string> header, char sep=',') {
    std::ofstream::open(path.c_str());
    setf(std::ios_base::fixed);
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
  CsvWriter ends;
  CsvWriter releases;
  CsvWriter attributes;
  CsvWriter resources;
};

#endif
