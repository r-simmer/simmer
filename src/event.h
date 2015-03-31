#ifndef EVENT_H
#define EVENT_H

//TODO: monitor activity (standard monitoring = on, but allow for disabling)

#include <stdlib.h>   
#include <memory>

// forward declarations
class Resource;
class Entity;



class Event
{
public:
	Entity* parent_entity;
	std::string description, type;

	int early_start_time, end_time;
	bool processing, finished;


	void set_parent_entity(Entity* ent);
	Resource* get_resource(std::string, Simulator*);
	
	virtual bool try_to_start(int) = 0;
	virtual bool stop(int) = 0;

	virtual ~Event() {};
	virtual Event* clone() const = 0;
	

	
};

class SkipEvent: public Event
{
	int n_events;
public:
	SkipEvent(Entity* parent, int n) {
		n_events = n;
		parent_entity = parent;
		end_time = -1;
		type = "SkipEvent";
	};

	virtual SkipEvent* clone() const { return new SkipEvent (*this); }

	virtual bool try_to_start(int now) {
		
		for(int i = 0; i < n_events; ++i){
			Event* event_to_delete = parent_entity->get_event();
			delete event_to_delete;
		}
		end_time = now;
		return true;
		
	}

	virtual bool stop(int now) {
		return true;
	}


};


class SeizeEvent: public Event
{
	std::string resource_name;

public:
	double resource_amount;
	bool enqueued;
	SeizeEvent(Entity* parent, std::string res, double res_amount) {
		parent_entity = parent;
		end_time = -1;
		resource_name = res;
		resource_amount = res_amount;
		type = "SeizeEvent";
		enqueued = false;
	};
	
	virtual SeizeEvent* clone() const { return new SeizeEvent (*this); }
	


	virtual bool try_to_start(int now) {
		
		Resource* resource = get_resource(resource_name, parent_entity->sim);
		int server_usage = resource->serve_mon->get_last_value();
		int queue_usage = resource->queue_mon->get_last_value();
		if(parent_entity->activation_time > now) return false;
		
		// another customer can be served
		if(resource->capacity >= server_usage + resource_amount) {
			// only if it was previously enqueued or there are no others waiting
			if(enqueued || !queue_usage) {
				// register seize
				resource->serve_mon->record_increment(now, resource_amount);
				if(enqueued) resource->queue_mon->record_decrement(now, 1);
				end_time = now;
				enqueued = false;
				return true;
			}
		}
		// this customer cannot be served now
		// already waiting
		if(enqueued) return false;
		// or enqueue
		if(resource->queue_size && (resource->queue_size < 0 || 
		  resource->queue_size > queue_usage)) {
			resource->queue_mon->record_increment(now, 1);
			enqueued = true;
		} else {
			// no room for it
			parent_entity->leave = true;
		}
		return false;
	}

	virtual bool stop(int now) {
		return true;
	}
};



class ReleaseEvent: public Event
{
	std::string resource_name;

public:

	virtual ~ReleaseEvent() {}

	double resource_amount;
	ReleaseEvent(Entity* parent,  std::string res, double res_amount) {
		parent_entity = parent;
		end_time = -1;
		resource_name = res;
		resource_amount = res_amount;
		type = "ReleaseEvent";

	};

	virtual ReleaseEvent* clone() const { return new ReleaseEvent (*this); }
	
	virtual bool try_to_start(int now) {
		if(parent_entity->activation_time > now) return false;
		
		Resource* resource = get_resource(resource_name, parent_entity->sim);
		if(resource->serve_mon->get_last_value() - resource_amount >= 0) {
			// register release
			resource->serve_mon->record_decrement(now, resource_amount);
			end_time = now;
			return true;
		} else {
			throw std::runtime_error("Error: trying to release more resources than capacity");
		}
	}

	virtual bool stop(int now) {
		return true;
	}
};




class TimeoutEvent: public Event
{

public:
	int duration;

	TimeoutEvent(Entity* parent, int time_units) {
		parent_entity = parent;
		end_time = -1;
		duration = time_units;
		type = "TimeoutEvent";

	};
	
	virtual TimeoutEvent* clone() const { return new TimeoutEvent (*this); }

	virtual bool try_to_start(int now) {
		if(parent_entity->activation_time > now) return false;
		parent_entity->monitor->record(now, 1);
		end_time = now + duration;
		return true; // timeout can always start

	};


	virtual bool stop(int now) {
		parent_entity->monitor->record(now, 0);
		return true;
	};




};




#endif
