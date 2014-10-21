#include <iostream>
#include <vector>
#include <ctime>
#include <memory>

#include "monitor.h"
#include "resource.h"

#include "entity.h"
#include "event.h"

#include "simulator.h"

#include "event_entity_glue.h"
#include "simulator_event_glue.h"


using std::cout;
using std::endl;



int main()
{
	int start_s=clock();
//	auto r1 = std::make_shared<Resource>("vpk",1);
//std::shared_ptr<Resource> r1 = r1(new Resource("vpk",1));

//    SeizeEvent *e1ev1 = new SeizeEvent(r1, 1);
//    TimeoutEvent *e1ev2 = new TimeoutEvent(10);
//    ReleaseEvent *e1ev3 = new ReleaseEvent("vpk",1);
//    r1->monitor->record(9,5);
//	std::cout << "415456:::: " << r1->monitor->get_last_value() << std::endl;
	Entity *e1 = new Entity("testEnt");
	e1->add_seize_event("vpk",1.0);
	e1->add_timeout_event(10);
	e1->add_timeout_event(10);
	e1->add_release_event("vpk", 1.0);

	for (int n=1; n<10; n++) {

//        e1->add_seize_event(r1,1.0);
//        e1->add_timeout_event(10);
//        e1->add_release_event(r1, 1.0);

	}

	Entity *e2 = new Entity("testEnt2");
	e2->add_seize_event("vpk",1.0);
	e2->add_timeout_event(10);
	e2->add_timeout_event(10);
	e2->add_release_event("vpk", 1.0);
	e2->add_timeout_event(1);
//	std::cout << "e2size: " << e2->event_list.size() << std::endl;

	Entity* e3 = new Entity(*e2);
	Entity* e4 = new Entity(*e3);
	cout << "aantal events: "<<e3->entity_event_vec.size() << endl;
	Simulator sim("testSim", true);

//	sim.add_resource(r1);
	sim.add_resource("vpk", 1);
	sim.add_entity(e1);
	sim.add_entity(e2);
	sim.add_entity(e3);
	sim.add_entity(e4);
	
	cout << e1->sim << " " << e1->sim->resource_map.size() <<  endl;
	cout << e3->sim << " " << e3->sim->resource_map.size() << endl;
	
	cout << e3->entity_event_vec[0]->parent_entity <<endl;
	cout << e1->entity_event_vec[0]->parent_entity <<endl;
	
	cout << (e1->sim == e3->sim) << endl;
//    std::cout << sim.get_resource("vpk")->name << std::endl;

//    std::cout << e1->event_list.size() << std::endl;
//	sim.initialize_events();
//	std::cout << "Events initialized" << std::endl;
//    std::cout << e1->event_list.size() << std::endl;

	sim.run();

//    std::cout << sim.entity_list[1]->monitor->get_last_value() << std::endl;
	int stop_s=clock();
	cout << endl << endl << "execution time: " << (stop_s-start_s)/double(CLOCKS_PER_SEC) << "s" << endl;
//	cout << "length resource map: " << sim.resources.size() << endl;
//	delete sim;
//	delete r1;
//	delete e1;
//	delete e2;

	return 0;

};
