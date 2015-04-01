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
	int queue_size;
	TimeValueMonitor* serve_mon;
	TimeValueMonitor* queue_mon;

	void seize(int);
	void release(int);




	Resource(std::string res_name, double res_capacity, int res_queue_size): 
	name(res_name), capacity(res_capacity), queue_size(res_queue_size), serve_mon(new TimeValueMonitor()), queue_mon(new TimeValueMonitor()) {
		serve_mon->record(0,0);
		queue_mon->record(0,0);
	}

	~Resource()
	{
		delete serve_mon;
		delete queue_mon;
	}

};

#endif
