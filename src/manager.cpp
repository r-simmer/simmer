#include <simmer/process/manager.h>
#include <simmer/simulator.h>

void Manager::run() {
  if (sim->verbose) Rcpp::Rcout <<
    FMT(10, right) << sim->now() << " |" <<
    FMT(12, right) << "manager: " << FMT(15, left) << name << "|" <<
    FMT(12, right) << "parameter: " << FMT(15, left) << param << "| " <<
    value[index] << std::endl;

  set(value[index]);
  index++;
  if (index == duration.size()) {
    if (period < 0)
      goto end;
    index = 1;
  }

  sim->schedule(duration[index], this, priority);
end:
  return;
}
