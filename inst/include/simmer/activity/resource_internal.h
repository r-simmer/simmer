// Copyright (C) 2016-2020 Iñaki Ucar
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

    ResGetter(const std::string& activity)
      : resource(MakeString() << "[all]"), id(-2), activity(activity) {}

    ResGetter(const std::string& activity, const std::string& resource)
      : resource(resource), id(-1), activity(activity) {}

    ResGetter(const std::string& activity, int id)
      : resource(MakeString() << "[" << id << "]"),
        id(std::abs(id)), activity(activity) {}

  protected:
    std::string resource;
    int id;

    Resource* get_resource(Arrival* arrival) const {
      Resource* selected = NULL;
      if (id == -2) return selected;
      if (id == -1)
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
    explicit Policy(const std::string& policy) : name(policy), state(new int(-1)),
      check_available(policy.find("-available") != std::string::npos)
    {
      policies["shortest-queue"]            = &Policy::policy_shortest_queue;
      policies["shortest-queue-available"]  = &Policy::policy_shortest_queue;
      policies["round-robin"]               = &Policy::policy_round_robin;
      policies["round-robin-available"]     = &Policy::policy_round_robin;
      policies["first-available"]           = &Policy::policy_first_available;
      policies["random"]                    = &Policy::policy_random;
      policies["random-available"]          = &Policy::policy_random;
    }

    Policy(const Policy& o) : name(o.name), state(o.state),
      check_available(o.check_available), policies(o.policies) {}

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
    SHD<int> state;
    bool check_available;
    MethodMap policies;

    Resource* policy_shortest_queue(Simulator* sim, const VEC<std::string>& resources) {
      Resource* selected = NULL;
      for (size_t i = 0; i < resources.size(); i++) {
        Resource* res = sim->get_resource(resources[i]);
        if (check_available && !res->get_capacity())
          continue;
        if (!selected ||
            ((selected->get_capacity() >= 0) &&
             ((res->get_capacity() < 0) ||
              (res->get_queue_count()
                 + res->get_server_count() - res->get_capacity() <
                   selected->get_queue_count()
                 + selected->get_server_count() - selected->get_capacity()))))
          selected = res;
      }
      if (!selected)
        Rcpp::stop("policy '%s' found no resource available", name);
      return selected;
    }

    Resource* policy_round_robin(Simulator* sim, const VEC<std::string>& resources) {
      Resource* selected;
      for (size_t i = 0; i < resources.size(); i++) {
        if (++(*state) >= (int)resources.size())
          *state = 0;
        selected = sim->get_resource(resources[*state]);
        if (!check_available || selected->get_capacity())
          goto select;
      }
      Rcpp::stop("policy '%s' found no resource available", name);
    select:
      return selected;
    }

    Resource* policy_first_available(Simulator* sim, const VEC<std::string>& resources) {
      Resource* selected, * first_available = NULL;
      for (size_t i = 0; i < resources.size(); i++) {
        selected = sim->get_resource(resources[i]);
        if (!first_available && selected->get_capacity())
          first_available = selected;
        if (selected->get_capacity() < 0 ||
            selected->get_server_count() < selected->get_capacity())
          goto select;
      }
      for (size_t i = 0; i < resources.size(); i++) {
        selected = sim->get_resource(resources[i]);
        if ((selected->get_queue_size() < 0 ||
            (selected->get_queue_count() < selected->get_queue_size())) &&
            (!check_available || selected->get_capacity()))
          goto select;
      }
      if (!first_available)
        Rcpp::stop("policy '%s' found no resource available", name);
      selected = first_available;
    select:
      return selected;
    }

    Resource* policy_random(Simulator* sim, const VEC<std::string>& resources) {
      VEC<Resource*> available;
      foreach_ (const std::string& name, resources) {
        Resource* res = sim->get_resource(name);
        if (!check_available || res->get_capacity())
          available.push_back(res);
      }
      if (!available.size())
        Rcpp::stop("policy '%s' found no resource available", name);
      Rcpp::RNGScope scope;
      return available[Rcpp::sample(available.size(), 1)[0] - 1];
    }
  };

}} // namespace internal simmer

#endif
