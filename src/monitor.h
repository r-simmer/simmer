#ifndef MONITOR_H
#define MONITOR_H

#include "simmer.h"

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
  void insert(const std::string& key, const T& value) {
    if (map.find(key) == map.end())
      map[key] = VEC<T>();
    boost::get< VEC<T> >(map[key]).push_back(value);
  }

  void clear() { map.clear(); }

private:
  _map map;
};

class Monitor {
public:
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

  virtual RData get_arrivals(bool per_resource) const = 0;
  virtual RData get_attributes() const = 0;
  virtual RData get_resources() const = 0;
};

class MemMonitor : public Monitor {
public:
  void clear() {
    arr_traj.clear();
    arr_res.clear();
    attributes.clear();
    resources.clear();
  }

  void record_end(const std::string& name, double start, double end,
                  double activity, bool finished)
  {
    arr_traj.insert("name",           name);
    arr_traj.insert("start_time",     start);
    arr_traj.insert("end_time",       end);
    arr_traj.insert("activity_time",  activity);
    arr_traj.insert("finished",       finished);
  }

  void record_release(const std::string& name, double start, double end,
                      double activity, const std::string& resource)
  {
    arr_res.insert("name",            name);
    arr_res.insert("start_time",      start);
    arr_res.insert("end_time",        end);
    arr_res.insert("activity_time",   activity);
    arr_res.insert("resource",        resource);
  }

  void record_attribute(double time, const std::string& name,
                        const std::string& key, double value)
  {
    attributes.insert("time",         time);
    attributes.insert("name",         name);
    attributes.insert("key",          key);
    attributes.insert("value",        value);
  }

  void record_resource(const std::string& name, double time, int server_count,
                       int queue_count, int capacity, int queue_size)
  {
    resources.insert("resource",      name);
    resources.insert("time",          time);
    resources.insert("server",        server_count);
    resources.insert("queue",         queue_count);
    resources.insert("capacity",      capacity);
    resources.insert("queue_size",    queue_size);
  }

  RData get_arrivals(bool per_resource) const {
    if (!per_resource) return RData::create(
      Rcpp::Named("name")             = arr_traj.get<std::string>("name"),
      Rcpp::Named("start_time")       = arr_traj.get<double>("start_time"),
      Rcpp::Named("end_time")         = arr_traj.get<double>("end_time"),
      Rcpp::Named("activity_time")    = arr_traj.get<double>("activity_time"),
      Rcpp::Named("finished")         = arr_traj.get<bool>("finished"),
      Rcpp::Named("stringsAsFactors") = false
    );
    return RData::create(
      Rcpp::Named("name")             = arr_res.get<std::string>("name"),
      Rcpp::Named("start_time")       = arr_res.get<double>("start_time"),
      Rcpp::Named("end_time")         = arr_res.get<double>("end_time"),
      Rcpp::Named("activity_time")    = arr_res.get<double>("activity_time"),
      Rcpp::Named("resource")         = arr_res.get<std::string>("resource"),
      Rcpp::Named("stringsAsFactors") = false
    );
  }

  RData get_attributes() const {
    return RData::create(
      Rcpp::Named("time")             = attributes.get<double>("time"),
      Rcpp::Named("name")             = attributes.get<std::string>("name"),
      Rcpp::Named("key")              = attributes.get<std::string>("key"),
      Rcpp::Named("value")            = attributes.get<double>("value"),
      Rcpp::Named("stringsAsFactors") = false
    );
  }

  RData get_resources() const {
    return RData::create(
      Rcpp::Named("resource")         = resources.get<std::string>("resource"),
      Rcpp::Named("time")             = resources.get<double>("time"),
      Rcpp::Named("server")           = resources.get<int>("server"),
      Rcpp::Named("queue")            = resources.get<int>("queue"),
      Rcpp::Named("capacity")         = resources.get<int>("capacity"),
      Rcpp::Named("queue_size")       = resources.get<int>("queue_size"),
      Rcpp::Named("stringsAsFactors") = false
    );
  }

private:
  MonitorMap arr_traj;     /**< arrival statistics per trajectory */
  MonitorMap arr_res;      /**< arrival statistics per resource */
  MonitorMap attributes;   /**< attribute statistics */
  MonitorMap resources;    /**< resource statistics */
};

class CsvMonitor : public Monitor {
  struct sepofstream {
    sepofstream(std::ofstream& os, std::string& sep) : os(os), sep(sep) {}
    std::ofstream& os;
    std::string& sep;

    template <class T>
    friend sepofstream& operator<<(sepofstream& con, const T& x) {
      con.os << x << con.sep;
      return con;
    }
  };

public:
  CsvMonitor(const std::string& arr_traj_path, const std::string& arr_res_path,
             const std::string& attributes_path, const std::string& resources_path,
             const RFn& csv_reader)
    : arr_traj_path(arr_traj_path), arr_res_path(arr_res_path),
      attributes_path(attributes_path), resources_path(resources_path),
      csv_reader(csv_reader), sep(",") { init(); }

  void clear() {
    close();
    init();
  }

  void flush() {
    arr_traj.flush();
    arr_res.flush();
    attributes.flush();
    resources.flush();
  }

  void record_end(const std::string& name, double start, double end,
                  double activity, bool finished)
  {
    sepofstream c(arr_traj, sep);
    c << name << start << end << activity;
    arr_traj << finished << "\n";
  }

  void record_release(const std::string& name, double start, double end,
                      double activity, const std::string& resource)
  {
    sepofstream c(arr_res, sep);
    c << name << start << end << activity;
    arr_res << resource << "\n";
  }

  void record_attribute(double time, const std::string& name,
                        const std::string& key, double value)
  {
    sepofstream c(attributes, sep);
    c << time << name << key;
    attributes << value << "\n";
  }

  void record_resource(const std::string& name, double time, int server_count,
                       int queue_count, int capacity, int queue_size)
  {
    sepofstream c(resources, sep);
    c << name << time << server_count << queue_count << capacity;
    resources << queue_size << "\n";
  }

  RData get_arrivals(bool per_resource) const {
    if (!per_resource)
      return csv_reader(arr_traj_path);
    return csv_reader(arr_res_path);
  }

  RData get_attributes() const { return csv_reader(attributes_path); }

  RData get_resources() const { return csv_reader(resources_path); }

private:
  std::string arr_traj_path;
  std::string arr_res_path;
  std::string attributes_path;
  std::string resources_path;
  std::ofstream arr_traj;          /**< arrival statistics per trajectory */
  std::ofstream arr_res;           /**< arrival statistics per resource */
  std::ofstream attributes;        /**< attribute statistics */
  std::ofstream resources;         /**< resource statistics */
  RFn csv_reader;
  std::string sep;

  void init() {
    arr_traj.open(arr_traj_path.c_str());
    arr_res.open(arr_res_path.c_str());
    attributes.open(attributes_path.c_str());
    resources.open(resources_path.c_str());
    sepofstream a(arr_traj, sep), b(arr_res, sep), c(attributes, sep), d(resources, sep);
    a << "name" << "start_time" << "end_time" << "activity_time";
    arr_traj << "finished\n";
    b << "name" << "start_time" << "end_time" << "activity_time";
    arr_res << "resource\n";
    c << "time" << "name" << "key";
    attributes << "value\n";
    d << "resource" << "time" << "server" << "queue" << "capacity";
    resources << "queue_size\n";
  }

  void close() {
    arr_traj.close();
    arr_res.close();
    attributes.close();
    resources.close();
  }
};

#endif
