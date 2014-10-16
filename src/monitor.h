#ifndef MONITOR_H
#define MONITOR_H

#include<vector>

class TimeValueMonitor
{
public:
	std::string name;

	std::vector<int> time_vec;
	std::vector<double> value_vec;

	TimeValueMonitor(std::string name_str = "anonymous"): name(name_str) {}


	void record(int time, double value) {
    time_vec.push_back(time);
		value_vec.push_back(value);

	}

	void record_increment(int time, double value) {
		record(time, get_last_value()+value);
	}

	void record_decrement(int time, double value) {
		record(time, get_last_value()-value);
	}

	double get_last_value() {

		return value_vec.back();
	}
  
  std::pair<std::vector<int>, std::vector<double> > get_all_values() {
    return std::make_pair(time_vec, value_vec);
    
  }

};





#endif
