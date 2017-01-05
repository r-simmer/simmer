#include "policy.h"
#include "simulator.h"

int rand_int(int min, int max) {
  return int(round(R::runif(0, 1) * (max - min + 1) + min - 0.5));
}

Resource* Policy::policy_shortest_queue(Simulator* sim) {
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

Resource* Policy::policy_round_robin(Simulator* sim) {
  static int i = -1;
  if (++i == (int)resources.size())
    i = 0;
  return sim->get_resource(resources[i]);
}

Resource* Policy::policy_first_available(Simulator* sim) {
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

Resource* Policy::policy_random(Simulator* sim) {
  int i = rand_int(0, resources.size()-1);
  return sim->get_resource(resources[i]);
}
