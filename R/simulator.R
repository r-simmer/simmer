ReplicationContainer<-setRefClass("ReplicationContainer", fields=list(
  simulators = "vector"))

ReplicationContainer$methods(build_container = function(sim_obj, n_replications = 1){
  # TODO; list/vectors binnen refClasses moeten apart gekopieerd worden
  simulators<<-sapply(1:n_replications, function(x) deep_copy_simulator(sim_obj))
  return(.self)
})


ReplicationContainer$methods(simmer = function(until = Inf){
  for(sim in .self$simulators){
    sim$simmer(until)
  }
  
  return(.self)
})


#' @export
replicator<-function(sim_obj, n_replications){
  replication_container<-ReplicationContainer()
  replication_container$build_container(sim_obj, n_replications)
}


deep_copy_simulator<-function(sim_obj){
  obj_copy <- sim_obj$copy()
  obj_copy$resources_monitor<-sapply(obj_copy$resources_monitor, function(obj) obj$copy())
  obj_copy
}

#' @export
simmer<-function(sim_obj, until=Inf) sim_obj$simmer(until)

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

Simulator$methods(init_events = function(){
  ## loop over entities
  for(i in 1:length(entities_name)){
    ## get trajectory
    traj<-trajectories[[entities_trajectory_name[[i]]]]
    #     print(traj)
    entity_event_list<-list()
    step <- traj[1,]
    while(TRUE){
      #       print(step)
      early_start <- ifelse(length(entity_event_list)==0, entities_early_start[[i]], -999)
      
      event_details<-list(
        id = as.character(step[["event_id"]]),
        description = as.character(step[["description"]]),
        resource = unlist(strsplit(as.character(step[["resource"]]),"/")),
        duration = floor(eval(parse(text=as.character(step[["duration"]])))),
        early_start_time = early_start,
        start_time = -999,
        end_time = -999
      )
      

      event_details$resources_fulfilled<-sapply(event_details$resource, function(x) FALSE, USE.NAMES=T)
      event_details$amount <- mapply(function(res_name, amount) amount, 
                                     event_details$resource, as.numeric(unlist(strsplit(as.character(step[["amount"]]),"/"))),
                                     SIMPLIFY=F, USE.NAMES=T)      


      successor_evaluated<-as.character(
        eval(parse(text=as.character(step[["successor"]])))
      )

      
      
      
      entity_event_list[[length(entity_event_list)+1]]<-event_details
      
      if(is.na(successor_evaluated)) break
      step<-subset(traj, as.character(traj$event_id)==successor_evaluated)
    }
    
    event_list[[length(event_list)+1]]<<-entity_event_list
    
    
    
  }
})

Simulator$methods(simmer = function(until = Inf){
  message("Initializing events")
  .self$init_events()
  message("...done")
  
  ## (first event for all entities was set to 1)
  ## get early_start_times
  # loop over current_events (=events running or waiting to start)
  sim_start<-Sys.time()
  message("Starting simulation")
  while(current_time < until && !all(is.na(entities_current_event))){
    
    result<-
      mapply(function(entity_i, event_i){ 
        
        
        if(!is.na(entities_current_event[[entity_i]])){
          ## if not all events have been finished

          if(event_list[[entity_i]][[event_i]]$end_time <= current_time && event_list[[entity_i]][[event_i]]$end_time>=0){
            # end time has been reached, start next activity
            # record releasing of resources
            for(res_name in event_list[[entity_i]][[event_i]]$resource){
              
              res_monitor <- resources_monitor[[res_name]]
              res_monitor$record(current_time, res_monitor$get_last_value() - event_list[[entity_i]][[event_i]]$amount[[res_name]])
            }
            
            ## record stop in entity monitor
            entities_monitor[[entity_i]]$record(current_time, 0)
            
            
            if(event_i + 1 <= length(event_list[[entity_i]])){
              entities_current_event[[entity_i]]<<-event_i + 1
            } else {
              entities_current_event[[entity_i]]<<-NA
            }
            
          }
          
          if(event_list[[entity_i]][[event_i]]$early_start_time <= current_time && event_list[[entity_i]][[event_i]]$start_time<0){
            # early start time has passed but event hasnt started yet
            # check if resources are available for it to start
            
            for(res_name in event_list[[entity_i]][[event_i]]$resource){
              res_monitor <- resources_monitor[[res_name]]
              if(res_monitor$get_last_value() + + event_list[[entity_i]][[event_i]]$amount[[res_name]] <= resources_capacity[[res_name]] 
                 && event_list[[entity_i]][[event_i]]$resources_fulfilled[[res_name]]!= TRUE){
                
                ## record seizing of resources -- toggle to in use
                res_monitor$record(current_time, res_monitor$get_last_value() + event_list[[entity_i]][[event_i]]$amount[[res_name]])
                event_list[[entity_i]][[event_i]]$resources_fulfilled[[res_name]]<<-TRUE
                
              }
              
            }
            if(all(event_list[[entity_i]][[event_i]]$resources_fulfilled)){
              ## if all resources fulfilled, start the event
              event_list[[entity_i]][[event_i]]$start_time<<-current_time
              event_list[[entity_i]][[event_i]]$end_time<<- current_time + event_list[[entity_i]][[event_i]]$duration
              ## record start in entity monitor
              entities_monitor[[entity_i]]$record(current_time, 1)
              
            }
            
            
          }
          
        }
        
      },
      1:length(entities_current_event), entities_current_event, SIMPLIFY=F)
    
    
    current_time <<- current_time + 1
  }
  
  message(paste0("Simulation ended at ", current_time, " (simulation time) and took ", Sys.time() - sim_start, " seconds"))
  
  
})



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





#' @export
TimeValueMonitor <- setRefClass("TimeValueMonitor",
                                fields = list(
                                  data = "data.frame")
)


TimeValueMonitor$methods(initialize = function(...){
  callSuper(...)
  .self$data <- data.frame(t=0, v=0)
  .self
})

TimeValueMonitor$methods(record = function(t, v){
  data<<-rbind(data, data.frame(t, v))
})

TimeValueMonitor$methods(get_last_value = function(t, v){
  return(data[nrow(data),"v"])
})

