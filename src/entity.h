#ifndef ENTITY_H
#define ENTITY_H

// forward declarations
#include <memory>

class Event;
class TimeoutEvent;
class SeizeEvent;
class Simulator;



class Entity
{
public:
	Entity(const Entity& other);

	std::string name;
	std::vector <Event*> entity_event_vec;
	TimeValueMonitor* monitor;
	int activation_time;
	Simulator* sim;
  
	Entity(std::string ent_name, int activation): name(ent_name), monitor(new TimeValueMonitor()), activation_time(activation) {
    // set activation time
    monitor->record(activation, -999);
	}

	void set_simulator(Simulator* sim_obj) {
		sim = sim_obj;
	}

	Event* get_event() {
		Event* ev = entity_event_vec.front();
		entity_event_vec.erase(entity_event_vec.begin());
		return ev;
	}

	~Entity();

	void add_timeout_event(int);
	void add_seize_event(std::string, double);
	void add_release_event(std::string, double);
  void add_skip_event(int);

};




#endif
