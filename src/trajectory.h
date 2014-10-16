    #ifndef TRAJECTORY_H
#define TRAJECTORY_H

#include "event.h"

class Trajectory {
public:
    std::string name;
    std::vector <Event*> event_list;
    Trajectory(std::string);
    void add_event(Event*);
    };

Trajectory::Trajectory(std::string tr_name){
    name = tr_name;
};


void Trajectory::add_event(Event* ev){
   event_list.push_back(ev);
};

#endif
