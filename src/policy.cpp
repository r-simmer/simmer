#include "policy.h"
#include "simulator.h"

#include <boost/random/mersenne_twister.hpp>
#include <boost/random/discrete_distribution.hpp>

boost::mt19937 mersenne_gen;

int rand_int(int min, int max) {
  boost::random::uniform_int_distribution<> dist(min, max);
  return dist(mersenne_gen);
}

Resource* Policy::policy_shortest_queue(Simulator* sim) {
  Resource* selected = sim->get_resource(resources[0]);
  int n = resources.size();
  for (int i = 1; i < n; i++) {
    Resource* res = sim->get_resource(resources[i]);
    if (res->get_server_count() + res->get_queue_count() <
      selected->get_server_count() + selected->get_queue_count())
      selected = res;
  }
  return selected;
}

Resource* Policy::policy_round_robin(Simulator* sim) {
  static int i = -1;
  if (++i == (int)resources.size())
    i = 0;
  return sim->get_resource(resources[i]);
}

Resource* Policy::policy_first_available(Simulator* sim) {
  int i, n = resources.size();
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

Resource* Policy::policy_random(Simulator* sim) {
  int i = rand_int(0, resources.size()-1);
  return sim->get_resource(resources[i]);
}
