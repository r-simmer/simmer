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
