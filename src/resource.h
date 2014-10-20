#ifndef RESOURCE_H
#define RESOURCE_H


// forward declarations
class TimeValueMonitor;

class Resource
{
	Resource(Resource& other);
public:
	std::string name;
	double capacity;
	TimeValueMonitor* monitor;

	void seize(int);
	void release(int);




	Resource(std::string res_name, double res_capacity): name(res_name), capacity(res_capacity), monitor(new TimeValueMonitor()) {
    monitor->record(0,0);
	}
  
	~Resource()
	{
		delete monitor;
	}

};

#endif
