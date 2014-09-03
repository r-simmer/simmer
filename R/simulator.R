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
  
  ## initialize activation times
  if(getOption("verbose")) message("Initializing activation times")
  entities_early_start<<-
    lapply(entities_early_start, function(start) eval(parse(text=start)))
  
  if(getOption("verbose")) message("...done")
  
  
  if(getOption("verbose")) message("Initializing events")
  
  
  ## loop over entities
  for(i in 1:length(entities_name)){
    ## get trajectory
    traj<-trajectories[[entities_trajectory_name[[i]]]]
    #     print(traj)
    entity_event_list<-list()
    step <- traj[1,]
    while(TRUE){
      
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
  if(getOption("verbose")) message("...done")
  
  
})

Simulator$methods(simmer = function(until = Inf){
  
  
  ## (first event for all entities was set to 1)
  ## get early_start_times
  # loop over current_events (=events running or waiting to start)
  sim_start<-Sys.time()
  if(getOption("verbose")) message("Starting simulation")
  while(current_time < until && !all(is.na(entities_current_event))){
    
    
    ## stop finished activities (and init next one)
    for(entity_i in 1:length(entities_current_event)){
      event_i<-entities_current_event[[entity_i]]
      
      
      if(!is.na(entities_current_event[[entity_i]])){
        ## if trajectory has not been finished
        
        if(event_list[[entity_i]][[event_i]]$end_time <= current_time && event_list[[entity_i]][[event_i]]$end_time>=0){
          # end time has been reached, start next activity and set early start of that activity
          # record releasing of resources
          for(res_name in event_list[[entity_i]][[event_i]]$resource){
            
            res_monitor <- resources_monitor[[res_name]]
            res_monitor$record(current_time, res_monitor$get_last_value() - event_list[[entity_i]][[event_i]]$amount[[res_name]])
          }
          
          ## record stop in entity monitor
          entities_monitor[[entity_i]]$record(current_time, 0)
          
          
          if(event_i + 1 <= length(event_list[[entity_i]])){
            entities_current_event[[entity_i]]<<-event_i + 1
            # set early start of started activity
            event_list[[entity_i]][[entities_current_event[[entity_i]]]]$early_start_time<<-current_time
          } else {
            entities_current_event[[entity_i]]<<-NA
          }
          
          
          
        }
      }
    }
    
    ## order activities based on early_start / end_time
    current_events_ordered<-order(
      sapply(1:length(entities_current_event), function(entity_index) {
        if(is.na(entities_current_event[[entity_index]])){ # trajectory of entity_index has finished
          return(Inf)
        } else if(event_list[[entity_index]][[entities_current_event[[entity_index]]]]$start_time>0){ #event has started
          return(event_list[[entity_index]][[entities_current_event[[entity_index]]]]$end_time)      
        } else{ #event is waiting to start
          return(event_list[[entity_index]][[entities_current_event[[entity_index]]]]$early_start_time)      
          
        }
        
      })
    )
    
    
    for(entity_i in current_events_ordered){
      event_i<-entities_current_event[[entity_i]]
      
      
      if(!is.na(entities_current_event[[entity_i]])){
        
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
    }
    
    
    current_time <<- current_time + 1 ## can be more efficient
  }
  
  # set register NA for all entities with unfinished trajectories
  sapply(1:length(entities_current_event), function(entity_id){
    if(!is.na(entities_current_event[[entity_id]])) entities_monitor[[entity_id]]$record(current_time, NA)
  })
  
  if(getOption("verbose")) message(paste0("Simulation ended at ", current_time, " (simulation time) and took ", Sys.time() - sim_start, " seconds"))
  
  return(.self)
})






