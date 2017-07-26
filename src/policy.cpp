#include "policy.h"
#include "simulator.h"

std::ostream& operator<<(std::ostream& out, const Policy& policy) {
  out << policy.name;
  return out;
}

Resource* Policy::policy_shortest_queue(Simulator* sim, const VEC<std::string>& resources) {
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

Resource* Policy::policy_round_robin(Simulator* sim, const VEC<std::string>& resources) {
  static int i = -1;
  if (++i >= (int)resources.size())
    i = 0;
  return sim->get_resource(resources[i]);
}

Resource* Policy::policy_first_available(Simulator* sim, const VEC<std::string>& resources) {
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

Resource* Policy::policy_random(Simulator* sim, const VEC<std::string>& resources) {
  int i = Rcpp::sample(resources.size(), 1)[0];
  return sim->get_resource(resources[i-1]);
}
