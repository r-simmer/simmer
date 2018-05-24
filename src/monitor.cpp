#include <simmer.h>

using namespace Rcpp;
using namespace simmer;

//[[Rcpp::export]]
SEXP MemMonitor__new() {
  return XPtr<MemMonitor>(new MemMonitor());
}

//[[Rcpp::export]]
SEXP CsvMonitor__new(const std::string& ends_path, const std::string& releases_path,
                     const std::string& attributes_path, const std::string& resources_path,
                     const std::string& sep)
{
  return XPtr<CsvMonitor>(
    new CsvMonitor(ends_path, releases_path, attributes_path, resources_path, sep[0]));
}

//[[Rcpp::export]]
DataFrame get_arrivals_(SEXP mon_, bool per_resource) {
  XPtr<MemMonitor> mon(mon_);
  return mon->get_arrivals(per_resource);
}

//[[Rcpp::export]]
DataFrame get_attributes_(SEXP mon_) {
  XPtr<MemMonitor> mon(mon_);
  return mon->get_attributes();
}

//[[Rcpp::export]]
DataFrame get_resources_(SEXP mon_) {
  XPtr<MemMonitor> mon(mon_);
  return mon->get_resources();
}
