#ifndef POLICY_H
#define POLICY_H

#include "simmer.h"

// forward declarations
class Simulator;
class Resource;

class Policy {
  typedef Resource* (Policy::*method)(Simulator*, VEC<std::string>);
  typedef UMAP<std::string, method> MethodMap;

public:
  Policy(std::string policy) : name(policy) {
    policies["shortest-queue"]    = &Policy::policy_shortest_queue;
    policies["round-robin"]       = &Policy::policy_round_robin;
    policies["first-available"]   = &Policy::policy_first_available;
    policies["random"]            = &Policy::policy_random;
  }

  friend std::ostream& operator<<(std::ostream& out, const Policy& policy);

  Resource* dispatch(Simulator* sim, VEC<std::string> resources) {
    MethodMap::iterator x = policies.find(name);
    if (x == policies.end()) Rcpp::stop("policy '%s' not supported (typo?)", name);
    return ((*this).*(x->second))(sim, resources);
  }

private:
  std::string name;
  MethodMap policies;

  Resource* policy_shortest_queue(Simulator* sim, VEC<std::string> resources);
  Resource* policy_round_robin(Simulator* sim, VEC<std::string> resources);
  Resource* policy_first_available(Simulator* sim, VEC<std::string> resources);
  Resource* policy_random(Simulator* sim, VEC<std::string> resources);
};

#endif
