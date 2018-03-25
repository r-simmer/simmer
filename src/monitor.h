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

class MemoryMon : public Monitor {
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

#endif
