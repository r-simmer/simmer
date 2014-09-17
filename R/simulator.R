Simulator<-setRefClass("Simulator",
                       fields = list(
                         name = "character",
                         current_time = "numeric",
                         
                         entities_name = "list",
                         entities_early_start = "list",
                         entities_current_event = "list",
                         entities_trajectory_name = "list",
                         entities_monitor = "list",
                         
                         resources_name = "list",
                         resources_capacity = "list",
                         resources_monitor = "list",
                         
                         trajectories = "list",
                         
                         event_list = "list"
                       ))

Simulator$methods(now = function() current_time)

Simulator$methods(init_sim = function(){
  
  ## calculate early start times of entities
  if(getOption("verbose")) message("Parsing, evaluating and flooring early start times of entities")
  entities_early_start<<-
    lapply(entities_early_start, function(start) floor(eval(parse(text=start))))
  
  if(getOption("verbose")) message("...done")
  
  
  if(getOption("verbose")) message("Initializing events")
  
  
  ## init events and evaluate successor_id and durations
  for(i in 1:length(entities_trajectory_name)){
    event_list[[i]]<<-lapply(trajectories[[entities_trajectory_name[[i]]]]@timeline, function(ev) evaluate_event(ev, entities_early_start[[i]]))
    
  }
  
  
  if(getOption("verbose")) message("...done")
  
  
  ## register start in process of entity (@ early_start)
  if(getOption("verbose")) message("Registering activation time of entities in monitor")
  
  for(ent_i in 1:length(entities_early_start)){
    entities_monitor[[ent_i]]$record(entities_early_start[[ent_i]], 1)
    entities_monitor[[ent_i]]$record(entities_early_start[[ent_i]], 0)
  }
  
  if(getOption("verbose")) message("...done")
  
  
})

Simulator$methods(simmer = function(until = Inf){
  
  ## (first event for all entities was set to 1)
  
  sim_start<-Sys.time()
  if(getOption("verbose")) message("Starting simulation")
  while(current_time < until && !all(is.na(entities_current_event))){
    
    
    ## loop over current events
    for(entity_i in 1:length(entities_current_event)){
      event_i<-entities_current_event[[entity_i]]
      
      
      ## if trajectory for entity_i has not been finished
      if(!is.na(entities_current_event[[entity_i]])){
        
        event_obj<-event_list[[entity_i]][[event_i]]  
        
        
        
        # if end time has been reached, stop current activity, start next activity (if there is one) and set early start of next activity
        if(event_list[[entity_i]][[event_i]]$end_time <= current_time){
          # if other events are available, start 'em
          event_list[[entity_i]][[event_i]]
        }
        
        # end_time has not been set, activity still has to start, try to start it
        else if(is.infinite(event_obj@end_time)){
          
          
          ## activate next event
          if(class(event_obj) == "EventTimeout"){
            event_obj@end_time <- current_time + event_obj@duration
            return(event_obj)
          } else if(class(event_obj) == "ReleaseResource"){
            res_monitor<-resources_monitor[[event_obj@resource_name]]
            res_monitor$record(current_time, res_monitor$get_last_value() - event_obj@resource_amount)
            event_obj@end_time <- current_time
          } else if(class(event_obj) == "SeizeResource"){
            res_monitor<-resources_monitor[[event_obj@resource_name]]
            
            # check if resource are available, seize if they are and set endtime, otherwise leave endtime blank
            if(res_monitor$get_last_value() + event_obj@resource_amount <= resources_capacity[[res_name]]){
              res_monitor$record(current_time, res_monitor$get_last_value() + event_obj@resource_amount)
              event_obj@end_time <- current_time
            }
            
          }
          
        }
      }
    }
  }
        
#        
#   
#   current_time <<- current_time + 1 ## can be more efficient
# }
# 
# # set register NA for all entities with unfinished trajectories
# sapply(1:length(entities_current_event), function(entity_id){
#   if(!is.na(entities_current_event[[entity_id]])) entities_monitor[[entity_id]]$record(current_time, NA)
# })
# 
# if(getOption("verbose")) message(paste0("Simulation ended at ", current_time, " (simulation time) and took ", Sys.time() - sim_start, " seconds"))
# 
# return(.self)
})






