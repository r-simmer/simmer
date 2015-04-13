#ifndef SIMULATOR_EVENT_GLUE_H
#define SIMULATOR_EVENT_GLUE_H


#include <stdlib.h>

// redundant code: this function should call to parent_entity->sim->get_resource(name)
// but a couple of things would need to be reconsidered
Resource* Event::get_resource(std::string res_name, Simulator* sim_obj)
{
	try {
		return sim_obj->resource_map[res_name];
	} catch (...) {
		// not found
		throw std::runtime_error("resource '" + res_name + "' not found (typo?)");
	}
}

#endif
