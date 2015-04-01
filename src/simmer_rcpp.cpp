
#include <Rcpp.h>


#include <iostream>
#include <vector>
#include <ctime>
#include <memory>
#include <utility>

#include "monitor.h"
#include "resource.h"

#include "entity.h"
#include "event.h"

#include "simulator.h"

#include "event_entity_glue.h"
#include "simulator_event_glue.h"

using namespace Rcpp;




//[[Rcpp::export]]
void add_timeout_event_(SEXP ent, SEXP duration_) {
  
  XPtr<Entity> ent_ptr(ent);
  int duration = as<int>(duration_);
  ent_ptr->add_timeout_event(duration);
  
}

//[[Rcpp::export]]
void add_seize_event_(SEXP ent, SEXP res_name_, SEXP amount_) {
  
  XPtr<Entity> ent_ptr(ent);
  std::string res_name = as<std::string>(res_name_);
  
  double amount = as<double>(amount_);
  ent_ptr->add_seize_event(res_name, amount);
  
}

//[[Rcpp::export]]
void add_skip_event_(SEXP ent, SEXP number_) {
  
  XPtr<Entity> ent_ptr(ent);
  
  int number = as<int>(number_);
  ent_ptr->add_skip_event(number);
  
}


//[[Rcpp::export]]
void add_release_event_(SEXP ent, SEXP res_name_, SEXP amount_) {
  
  XPtr<Entity> ent_ptr(ent);
  std::string res_name = as<std::string>(res_name_);
  
  double amount = as<double>(amount_);
  ent_ptr->add_release_event(res_name, amount);
  
}

//[[Rcpp::export]]
void add_entity_(SEXP sim, SEXP ent) {
  
  XPtr<Entity> ent_ptr(ent);
  XPtr<Simulator> sim_ptr(sim);
  
  sim_ptr->add_entity(ent_ptr);
}




//[[Rcpp::export]]
void add_resource_(SEXP sim, SEXP name_, SEXP capacity_, SEXP queue_size_){
  XPtr<Simulator> sim_ptr(sim);
  
  int capacity = as<int>(capacity_);
  int queue_size = as<int>(queue_size_);
  std::string name = as<std::string>(name_);
  
  sim_ptr->add_resource(name, capacity, queue_size);
}

//[[Rcpp::export]]
int get_resource_capacity_(SEXP sim, SEXP name_){
  XPtr<Simulator> sim_ptr(sim);
  std::string name = as<std::string>(name_);
  
  return sim_ptr->get_resource(name)->capacity;
}

//[[Rcpp::export]]
int get_resource_queue_size_(SEXP sim, SEXP name_){
  XPtr<Simulator> sim_ptr(sim);
  std::string name = as<std::string>(name_);
  
  return sim_ptr->get_resource(name)->queue_size;
}

//[[Rcpp::export]]
SEXP get_entity_monitor_values_(SEXP sim) {
  XPtr<Simulator> sim_ptr(sim);
  std::vector<int> time_vec;
  std::vector<double> value_vec;
  std::vector<int> entity_id_vec;
  std::vector<std::string> entity_name_vec;
  
  
  std::pair<std::vector<int>, std::vector<double> > values;
  for(unsigned int i = 0; i < sim_ptr->entity_vec.size(); ++i){
    values = sim_ptr->entity_vec[i]->monitor->get_all_values();
    
    for(unsigned int j = 0; j < values.first.size(); ++j){
      entity_id_vec.push_back(i);
      entity_name_vec.push_back( sim_ptr->entity_vec[i]->name );
      time_vec.push_back(values.first[j]);
      value_vec.push_back(values.second[j]);
      
    }
    
  };
  
  
  return Rcpp::List::create(Rcpp::Named("time") = time_vec,
  Rcpp::Named("value") = value_vec,
  Rcpp::Named("entity_id") = entity_id_vec,
  Rcpp::Named("entity_name") = entity_name_vec);
}


//[[Rcpp::export]]
SEXP get_resource_serve_mon_values_(SEXP sim, SEXP resource_name_) {
  XPtr<Simulator> sim_ptr(sim);
  std::string resource_name = as<std::string>(resource_name_);
  
  std::vector<int> time_vec;
  std::vector<double> value_vec;
  std::vector<std::string> resource_vec;
  
  TimeValueMonitor* res_mon = sim_ptr->get_resource(resource_name)->serve_mon;
  
  for(unsigned int i = 0; i < res_mon->value_vec.size(); ++i){
    time_vec.push_back(res_mon->time_vec[i]);
    value_vec.push_back(res_mon->value_vec[i]);
    resource_vec.push_back(resource_name);
    
  };
  
  
  return Rcpp::List::create(Rcpp::Named("time") = time_vec,
  Rcpp::Named("value") = value_vec,
  Rcpp::Named("resource") = resource_vec);
  
}

//[[Rcpp::export]]
SEXP get_resource_queue_mon_values_(SEXP sim, SEXP resource_name_) {
  XPtr<Simulator> sim_ptr(sim);
  std::string resource_name = as<std::string>(resource_name_);
  
  std::vector<int> time_vec;
  std::vector<double> value_vec;
  std::vector<std::string> resource_vec;
  
  TimeValueMonitor* res_mon = sim_ptr->get_resource(resource_name)->queue_mon;
  
  for(unsigned int i = 0; i < res_mon->value_vec.size(); ++i){
    time_vec.push_back(res_mon->time_vec[i]);
    value_vec.push_back(res_mon->value_vec[i]);
    resource_vec.push_back(resource_name);
    
  };
  
  
  return Rcpp::List::create(Rcpp::Named("time") = time_vec,
  Rcpp::Named("value") = value_vec,
  Rcpp::Named("resource") = resource_vec);
  
}

////[[Rcpp::export]]
//SEXP copy_entity_(SEXP original_entity){
//  Rcpp::XPtr<Entity> original_ptr(original_entity);
//  Rcpp::XPtr<Entity> copy_ptr(new Entity(*original_ptr), false );
//  return( copy_ptr );
//}

//[[Rcpp::export]]
void run_(SEXP sim) {
  try {
    XPtr<Simulator> sim_ptr(sim);
    sim_ptr->run();  
  } catch (std::exception &ex) {  
    forward_exception_to_r(ex);
  } catch(...) { 
    ::Rf_error("c++ exception (unknown reason)"); 
  }
}

//[[Rcpp::export]]
SEXP Simulator__new(SEXP name_, SEXP until_, SEXP verbose_) {
  
  std::string name = as<std::string>(name_);
  int until = as<int>(until_);
  bool verbose = as<bool>(verbose_);
  
  if(until>0){
    Rcpp::XPtr<Simulator> ptr(new Simulator( name, until , verbose), false );
    return ptr;
  }
  else 
  {
    Rcpp::XPtr<Simulator> ptr(new Simulator( name , verbose), false);
    return ptr;
  }
  
}

//' Return the current time of a simulator
//'
//' @param sim a external pointer to a simulator object
//' @export
//[[Rcpp::export]]
int now_(SEXP sim) {
  XPtr<Simulator> sim_ptr(sim);
  return sim_ptr->now();
}
    
    
//[[Rcpp::export]]
SEXP Entity__new(SEXP name_, SEXP activation_time_) {
  
  std::string name = as<std::string>(name_);
  int activation_time = as<int>(activation_time_);
  
  Rcpp::XPtr<Entity> ptr( new Entity( name, activation_time), false );
  return ptr;
  
}


