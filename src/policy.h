#ifndef POLICY_H
#define POLICY_H

#include "simmer.h"

// forward declarations
class Simulator;
class Resource;

class Policy {
  typedef Resource* (Policy::*method)(Simulator*);
  typedef UMAP<std::string, method> MethodMap;
  
public:
  std::string name;
  
  Policy(Rcpp::Function resources, std::string policy) {}
  Policy(VEC<std::string> resources, std::string policy)
    : name(policy), resources(resources)
  {
    policies["shortest-queue"]    = &Policy::policy_shortest_queue;
    policies["round-robin"]       = &Policy::policy_round_robin;
    policies["first-available"]   = &Policy::policy_first_available;
    policies["random"]            = &Policy::policy_random;
  }
  
  Resource* dispatch(Simulator* sim) {
    MethodMap::iterator x = policies.find(name);
    if (x == policies.end())
      Rcpp::stop("policy '" + name + "' not supported (typo?)");
    return ((*this).*(x->second))(sim);
  }
  
private:
  VEC<std::string> resources;
  MethodMap policies;
  
  Resource* policy_shortest_queue(Simulator* sim);
  Resource* policy_round_robin(Simulator* sim);
  Resource* policy_first_available(Simulator* sim);
  Resource* policy_random(Simulator* sim);
};

#endif
