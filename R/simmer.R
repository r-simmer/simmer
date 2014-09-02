#' @export
simmer<-function(sim_obj, until=Inf) sim_obj$simmer(until)


#' @export
add_resource<-function(sim_obj, name, capacity=1){
  #   sim_obj$resources_name[[length(sim_obj$resources_name)+1]] <- name 
  sim_obj$resources_capacity[[name]] <- capacity
  sim_obj$resources_monitor[[name]]<- TimeValueMonitor()
  
  return(sim_obj)
}

#' @export
create_simulator<-function(name = "anonymous"){
  Simulator$new(name=name, current_time=0)
}

#' @export
add_trajectory<-function(sim_obj, name, trajectory_df){
  sim_obj$trajectories[[name]]<-trajectory_df
  
  return(sim_obj)
}
#' @export
add_entity<-function(sim_obj, name, trajectory_name, early_start = 0){
  sim_obj$entities_name[[length(sim_obj$entities_name)+1]]<-name
  sim_obj$entities_trajectory_name[[length(sim_obj$entities_trajectory_name)+1]]<-trajectory_name
  sim_obj$entities_early_start[[length(sim_obj$entities_early_start)+1]]<-early_start
  sim_obj$entities_monitor[[length(sim_obj$entities_monitor)+1]]<-TimeValueMonitor()
  # register start in process of event (@ early_start)
  sim_obj$entities_monitor[[length(sim_obj$entities_monitor)]]$record(early_start, 1)
  sim_obj$entities_monitor[[length(sim_obj$entities_monitor)]]$record(early_start, 0)
  
  sim_obj$entities_current_event[[length(sim_obj$entities_current_event)+1]]<-1
  
  return(sim_obj)
}


#' @export
add_entities_with_interval<-function(sim_obj, n, name_prefix, trajectory_name, interval, start_time=0){
  time <- start_time
  
  for(x in 1:n){
    add_entity(sim_obj, paste0(name_prefix,x), early_start = time, trajectory_name = trajectory_name)    
    time <- time + eval(substitute(interval))
  }
  
  return(sim_obj)
  
}