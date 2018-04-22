#include "task.h"
#include "simulator.h"

void Task::run() {
  if (sim->verbose) Rcpp::Rcout <<
    FMT(10, right) << sim->now() << " |" <<
    FMT(12, right) << "task: " << FMT(15, left) << name << "|" <<
    FMT(12+16, right) << "|" << std::endl;

  task();
  delete this;
}
