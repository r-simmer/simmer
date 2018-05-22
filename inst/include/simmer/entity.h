#ifndef simmer__entity_h
#define simmer__entity_h

#include <simmer/common.h>
#include <simmer/simulator.h>

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
