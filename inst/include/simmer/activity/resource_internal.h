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

#ifndef simmer__activity_resource_internal_h
#define simmer__activity_resource_internal_h

#include <simmer/common.h>
#include <simmer/simulator.h>
#include <simmer/resource.h>

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

  class Policy {
    typedef Resource* (Policy::*method)(Simulator*, const VEC<std::string>&);
    typedef UMAP<std::string, method> MethodMap;

  public:
    explicit Policy(const std::string& policy) : name(policy) {
      policies["shortest-queue"]    = &Policy::policy_shortest_queue;
      policies["round-robin"]       = &Policy::policy_round_robin;
      policies["first-available"]   = &Policy::policy_first_available;
      policies["random"]            = &Policy::policy_random;
    }

    friend std::ostream& operator<<(std::ostream& out, const Policy& policy) {
      out << policy.name;
      return out;
    }

    Resource* dispatch(Simulator* sim, const VEC<std::string>& resources) {
      MethodMap::iterator x = policies.find(name);
      if (x == policies.end()) Rcpp::stop("policy '%s' not supported (typo?)", name);
      return ((*this).*(x->second))(sim, resources);
    }

  private:
    std::string name;
    MethodMap policies;

    Resource* policy_shortest_queue(Simulator* sim, const VEC<std::string>& resources) {
      Resource* selected = sim->get_resource(resources[0]);
      size_t n = resources.size();
      for (size_t i = 1; i < n; i++) {
        Resource* res = sim->get_resource(resources[i]);
        if (res->get_server_count() + res->get_queue_count() <
          selected->get_server_count() + selected->get_queue_count())
          selected = res;
      }
      return selected;
    }

    Resource* policy_round_robin(Simulator* sim, const VEC<std::string>& resources) {
      static int i = -1;
      if (++i >= (int)resources.size())
        i = 0;
      return sim->get_resource(resources[i]);
    }

    Resource* policy_first_available(Simulator* sim, const VEC<std::string>& resources) {
      size_t i, n = resources.size();
      Resource* selected;
      for (i = 0; i < n; i++) {
        selected = sim->get_resource(resources[i]);
        if (selected->get_server_count() < selected->get_capacity())
          goto select;
      }
      for (i = 0; i < n; i++) {
        selected = sim->get_resource(resources[i]);
        if (selected->get_queue_count() < selected->get_queue_size())
          goto select;
      }
      return sim->get_resource(resources[0]);
      select:
        return selected;
    }

    Resource* policy_random(Simulator* sim, const VEC<std::string>& resources) {
      int i = Rcpp::sample(resources.size(), 1)[0];
      return sim->get_resource(resources[i-1]);
    }
  };

}} // namespace internal simmer

#endif
