#ifndef SIMULATOR_EVENT_GLUE_H
#define SIMULATOR_EVENT_GLUE_H


#include <stdlib.h>


Resource* Event::get_resource(std::string res_name, Simulator* sim_obj)
{
	
	// check if key exists
	if ( sim_obj->resource_map.find(res_name) == sim_obj->resource_map.end() ) {
		// not found
		throw std::runtime_error("Requested resource not found (typo?)");
	} else {
		// found
		return sim_obj->resource_map[res_name];
	}

}

#endif
