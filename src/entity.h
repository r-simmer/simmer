#ifndef ENTITY_H
#define ENTITY_H

#include "simmer.h"

#define SUCCESS 0
#define ENQUEUED -1
#define REJECTED -2

// forward declarations
class Simulator;

/**
 *  Base class. Every element in a simulation model is an entity.
 */
class Entity {
public:
  Simulator* sim;
  std::string name;

  Entity(Simulator* sim, std::string name, int mon)
    : sim(sim), name(name), mon(std::abs(mon)) {}
  virtual ~Entity() {}
  virtual void reset() = 0;
  int is_monitored() { return mon; }

private:
  int mon;
};

#endif
