#ifndef ENTITY_H
#define ENTITY_H

#include "simmer.h"

// forward declarations
class Simulator;

/**
 *  Base class. Every element in a simulation model is an entity.
 */
class Entity {
public:
  Simulator* sim;
  std::string name;

  Entity(Simulator* sim, const std::string& name, int mon)
    : sim(sim), name(name), mon(std::abs(mon)) {}
  virtual ~Entity() {}
  virtual void reset() = 0;
  int is_monitored() const { return mon; }

private:
  int mon;
};

#endif
