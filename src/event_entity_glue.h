#ifndef EVENT_ENTITY_GLUE_H
#define EVENT_ENTITY_GLUE_H

void Entity::add_timeout_event(int duration)
{
	TimeoutEvent* ev = new TimeoutEvent(this, duration);
	entity_event_vec.push_back(ev);
}

void Entity::add_seize_event(std::string res, double amount)
{
	SeizeEvent* ev = new SeizeEvent(this, res, amount);
	entity_event_vec.push_back(ev);

}

void Entity::add_release_event(std::string res, double amount)
{
	ReleaseEvent* ev = new ReleaseEvent(this, res, amount);
	entity_event_vec.push_back(ev);

}

void Entity::add_skip_event(int number)
{
  SkipEvent* ev = new SkipEvent(this, number);
	entity_event_vec.push_back(ev);

}

Entity::~Entity()
{
	delete monitor;
	while(!entity_event_vec.empty()) delete entity_event_vec.back(), entity_event_vec.pop_back();
}

Entity::Entity(const Entity& other): entity_event_vec(other.entity_event_vec.size())
{

	monitor = new TimeValueMonitor(*other.monitor);

	name = other.name;
	activation_time = other.activation_time;
	
	for (std::size_t i = 0; i < other.entity_event_vec.size(); ++i) {

		entity_event_vec[i] = other.entity_event_vec[i]->clone();
		entity_event_vec[i]->parent_entity = this;
		
	}



}


#endif
