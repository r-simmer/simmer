#' Start the simulation
#' @param sim_obj the Simulator object
#' @param until the maximum simulation time (after which the simulation is stopeed)
#' @export
simmer<-function(sim_obj, until=Inf){
  sim_obj$init_sim()
  sim_obj$simmer(until)
}

#' Add a resource
#' @param sim_obj the Simulator object
#' @param name specify the name of the resource
#' @param capacity the capacity of the resource
#' @export
add_resource<-function(sim_obj, name, capacity=1){
  #   sim_obj$resources_name[[length(sim_obj$resources_name)+1]] <- name 
  sim_obj$resources_capacity[[name]] <- capacity
  sim_obj$resources_monitor[[name]]<- TimeValueMonitor()
  
  return(sim_obj)
}

#' Create a Simulator object
#' @param name the name of the Simulator object
#' @export
create_simulator<-function(name = "anonymous"){
  Simulator$new(name=name, current_time=0)
}

#' Add a trajectory
#' @param sim_obj the Simulator object
#' @param name the name of the trajectory
#' @param trajectory_df the trajectory data frame
#' @export
add_trajectory<-function(sim_obj, name, trajectory_obj){
  sim_obj$trajectories[[name]]<-trajectory_obj
  
  return(sim_obj)
}

#' Add an individual entity
#' @param sim_obj the Simulator object
#' @param name the name of the entity
#' @param trajectory_name the name of the trajectory to use
#' @param early_start the time at which the entity is activated
#' @export
add_entity<-function(sim_obj, name, trajectory_name, early_start = 0){
  sim_obj$entities_name[[length(sim_obj$entities_name)+1]]<-name
  sim_obj$entities_trajectory_name[[length(sim_obj$entities_trajectory_name)+1]]<-trajectory_name
  sim_obj$entities_early_start[[length(sim_obj$entities_early_start)+1]]<-early_start
  sim_obj$entities_monitor[[length(sim_obj$entities_monitor)+1]]<-TimeValueMonitor()
    
  sim_obj$entities_current_event[[length(sim_obj$entities_current_event)+1]]<-1
  
  return(sim_obj)
}

#' Add multiple entities at once
#' @param sim_obj the Simulator object
#' @param n the number of entities to add
#' @param name_prefix the name prefix to use for the new entitites
#' @param trajectory_name the name of the trajectory to use
#' @param interval the time between activations of each entity (is evaluated as an R command)
#' @param start_time the time at which the first entity is activated
#' @export
add_entities_with_interval<-function(sim_obj, n, name_prefix, trajectory_name, interval, start_time=0){
  time <- start_time
  
  for(x in 1:n){
    add_entity(sim_obj, paste0(name_prefix,x), early_start = time, trajectory_name = trajectory_name)    
    time <- paste(time, interval, sep=" + ")
  }
  
  return(sim_obj)
  
}