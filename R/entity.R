setClass("Entity", representation(pointer = "externalptr"))
setMethod( "initialize", "Entity", function(.Object, name, activation_time) {
  .Object@pointer <- Entity__new(name, activation_time)
  .Object
} )



#' Creates an entity object
#' 
#' @param entity_name the name of the entity (defaults to 'anonymous')
#' @export
create_entity<-function(name = "anonymous", activation_time = 0){
  new("Entity", name, activation_time)
}


#' Add n entities with specified interval
#' 
#' @param sim_obj the simulator object
#' @param trajectory a trajectory object
#' @param name the name of the entities, with be suffixed by a number (1:n)
#' @param n the number of entities to create
#' @param activation_time the time at which to activate the first entity
#' @param interval the time between intervals, if a character value is given - e.g. "rnorm(1,10)" - it will be evaluated for every entity 
#' @export
add_entities_with_interval<-function(sim_obj, trajectory, name_prefix = "anonymous", n = 1, activation_time = 0, interval = 0){
  act_time <- activation_time
  for(i in 1:n){
    add_entity(sim_obj, name = paste0(name_prefix,"_",i), trajectory, activation_time = act_time)
    act_time <- act_time + evaluate_value(interval)
  }
  
  sim_obj
}

#' @export
add_entity<-function(sim_obj, trajectory, name = "anonymous", activation_time = 0){
  
  
  for(sim_ptr in sim_obj@simulators){
    
    ent<-
      create_entity(name, activation_time)
    
    for(ev in trajectory@events){

      if(ev$type == "SeizeEvent"){
        add_seize_event_to_entity(ent, evaluate_value(ev$resource), evaluate_value(ev$amount))
      } else if (ev$type == "ReleaseEvent"){
        add_release_event_to_entity(ent, evaluate_value(ev$resource), evaluate_value(ev$amount))
      } else if( ev$type == "TimeoutEvent"){
        add_timeout_event_to_entity(ent, floor(evaluate_value(ev$duration)))
      }
      
    }
    
    add_entity_(sim_ptr, ent@pointer)
    
  }
  
  sim_obj
}




#' @export
copy_entity<-function(entity_obj){
  entity_obj@pointer <- copy_entity_(entity_obj@pointer)
  entity_obj
}