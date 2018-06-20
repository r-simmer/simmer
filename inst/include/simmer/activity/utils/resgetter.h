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

#ifndef simmer__activity_utils_resgetter_h
#define simmer__activity_utils_resgetter_h

#include <simmer/simulator.h>

namespace simmer { namespace internal {

  // abstract class for resource retrieval
  class ResGetter {
  public:
    BASE_CLONEABLE(ResGetter)

    ResGetter(const std::string& activity, const std::string& resource, int id = -1)
      : resource(resource), id(id), activity(activity) {}

  protected:
    std::string resource;
    int id;

    Resource* get_resource(Arrival* arrival) const {
      Resource* selected = NULL;
      if (id < 0)
        selected = arrival->sim->get_resource(resource);
      else selected = arrival->get_resource_selected(id);
      if (!selected) Rcpp::stop("no resource selected");
      return selected;
    }

  private:
    std::string activity;
  };

}} // namespace internal simmer

#endif
