
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
                         
                         events_name = "list",
                         events_early_start_time = "list",
                         events_start_time = "list",
                         events_end_time = "list",
                         events_required_resources = "list",
                         events_successor = "list",
                         events_entity = "list",
                         event_list = "list"
                       ))

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
        amount = as.numeric(unlist(strsplit(as.character(step[["amount"]]),"/"))),
        duration = floor(eval(parse(text=as.character(step[["duration"]])))),
        early_start_time = early_start,
        start_time = NA,
        end_time = NA
      )
      
      event_details$resources_fulfilled<-sapply(1:length(event_details$resource), function(x) FALSE)
      
      successor_evaluated<-as.character(
        eval(parse(text=as.character(step[["successor"]])))
      )
#       print(successor_evaluated)
      
      
      
      entity_event_list[[length(entity_event_list)+1]]<-event_details
      
      if(is.na(successor_evaluated)) break
      step<-subset(traj, as.character(traj$event_id)==successor_evaluated)
    }
    
    event_list[[length(event_list)+1]]<<-entity_event_list
    
    
    
  }
})

# Simulator$methods(simmer = function(until = Inf){
#   # initialize all 
#   
# })

#' @export
add_resource<-function(sim_obj, name, capacity=1){
  sim_obj$resources_name[[length(sim_obj$resources_name)+1]] <- name 
  sim_obj$resources_capacity[[length(sim_obj$resources_capacity)+1]] <- capacity
  sim_obj$resources_monitor[[length(sim_obj$resources_monitor)+1]]<- TimeValueMonitor()
  
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
  sim_obj$entities_current_event[[length(sim_obj$entities_current_event)+1]]<--999
  
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


# 
# ### SIMULATOR CLASS
# 
# Simulator<-setRefClass("Simulator",
#                        fields = list(
#                          name = "character",
#                          entities = "vector",
#                          current_time = "numeric",
#                          resources = "vector",
#                          trajectories = "vector",
#                          events = "vector"))
# 
# Simulator$methods(initialize = function(...){
#   callSuper(...)
#   .self$events <- vector()
#   .self
# })
# 
# Simulator$methods(now = function() current_time)
# 
# Simulator$methods(seize_resources = function(evt){
#   for(req in evt$required_resources){
#     in_use <- req$resource_obj$monitor$get_last_value()
#     if(in_use + req$amount <= req$resource_obj$capacity && req$fulfilled == FALSE){
#       req$resource_obj$monitor$record(now(), in_use + req$amount)
#       req$fulfilled<-TRUE
#     }
#   }
#   # return whether all resource requirements are fullfilled or not
#   all(
#     sapply(evt$required_resources, function(req) req$fulfilled)
#   )
#   
# })
# 
# Simulator$methods(release_resources = function(evt){
#   for(req in evt$required_resources){
#     in_use <- req$resource_obj$monitor$get_last_value()
#     
#     req$resource_obj$monitor$record(now(), in_use - req$amount)
#     
#   }
# })
# 
# 
# 
# 
# 
# Simulator$methods(add_entity = function(name, trajectory, start_time=0){
#   
#   new_entity<-
#     Entity(name=name, 
#            early_start_time=start_time, 
#            trajectory=trajectory, 
#            trajectory_index = .self$get_trajectory_index(trajectory), 
#            entity_index = length(entities)+1)
#   entities<<-c(entities, 
#                new_entity)
# })
# 
# Simulator$methods(add_trajectory = function(name, trajectory){
#   trajectories<<-c(trajectories,
#                    new("trajectory", name=name, trajectory=trajectory)
#   )
# })
# 
# 
# Simulator$methods(get_trajectory_index = function(name){
#   Position(function(tr) tr@name == name, trajectories)
#   
#   
# })
# 
# 
# Simulator$methods(get_resource = function(name){
#   Filter(function(res) res$name == name, resources)[[1]]
# })
# 
# 
# 
# Simulator$methods(get_next_event_data = function(entity_index) {
#   
#   trajectory <- .self$trajectories[[.self$entities[[entity_index]]$trajectory_index]]@trajectory
#   
#   if(length(.self$entities[[entity_index]]$current_event$start_time)>0){ # there is already a step running / has runned
#     successor_id <- trajectory[trajectory$event_id == .self$entities[[entity_index]]$current_event$event_id, "successor"]
#     successor_id_evaluated<-
#       as.character(eval(parse(text=as.character(successor_id))))
#     #     print(successor_id_evaluated)
#     if(is.na(successor_id_evaluated)) {
#       return(NULL)
#     } else {
#       next_event_data <-  trajectory[trajectory$event_id == successor_id_evaluated, ]  
#       return(next_event_data)
#     }
#   } else { # first event of entity still has to start
#     
#     next_event_data<-trajectory[1,]
#   }
#   return(next_event_data)
# }
# )
# 
# 
# Simulator$methods(create_next_event = function(entity_index){ 
#   next_event <- .self$get_next_event_data(entity_index)
#   entity <- .self$entities[[entity_index]]
#   
#   if(!is.null(next_event)){ ## if FALSE, end of trajectory is reached
#     duration_evaluated <-
#       floor(eval(parse(text=as.character(next_event$duration[[1]]))))
#     successor_evaluated <-
#       as.character(eval(parse(text=as.character(next_event$successor[[1]]))))
#     
#     
#     res <- unlist(strsplit(as.character(next_event$resource),"/"))
#     amounts <- unlist(strsplit(as.character(next_event$amount),"/"))
#     
#     
#     res_reqs <- 
#       mapply(function(r,a){
#         ResourceRequirement(name = r, amount = as.numeric(a), resource_obj = .self$get_resource(r))
#       }, res, amounts, SIMPLIFY = T, USE.NAMES = F)
#     
#     
#     
#     new_evt = Event(event_id=as.character(next_event$event_id), 
#                     entity_name = entity$name,
#                     entity_index = entity_index,
#                     description=paste0(as.character(next_event$description), "__", entity$name), 
#                     required_resources = res_reqs, 
#                     duration=duration_evaluated,
#                     early_start = ifelse(entity$early_start_time > .self$now(), entity$early_start_time, .self$now()),
#                     successor=successor_evaluated)
#     
#     events <<- c(events, new_evt)
#     events <<- order_objects_by_slot_value(events, slot = "early_start")
#     
#     if(length(entity$current_event$early_start)>0){ # if current_event is not none
#       # stop current event
#       .self$stop_event(entity$current_event)
#     }
#     
#     
#     
#     entity$current_event <- new_evt
#     
#     .self$start_event(new_evt)
#     
#     return(new_evt)  
#   } else { # record stop of current event
#     .self$stop_event(entity$current_event)
#     
#   }
#   
#   return(NULL)
#   
# })
# 
# Simulator$methods(goto_next_step = function (){
#   if(length(.self$events)>0){
#     current_time <<- min(
#       sapply(.self$events, function(obj){
#         if(obj$has_started()) {
#           return(obj$end_time)
#         } else {
#           return(ifelse(obj$early_start>now(), obj$early_start, now()+1))
#         }
#       }
#       )
#     )
#   }
#   
# })
# 
# Simulator$methods(start_event = function(evt){ #rename naar start_event
#   if(.self$now() >= evt$early_start){
#     
#     # if all required resources are seized, start
#     if(.self$seize_resources(evt)){
#       
#       evt$start_time <- .self$now()
#       evt$end_time <- evt$start_time + evt$duration
#       .self$entities[[evt$entity_index]]$time_value_monitor$record(now(), 1) # record start of processing of event
#       return(TRUE)
#     }
#   }
#   return(FALSE)
# })
# 
# 
# Simulator$methods(plot_resource_usage = function(resource_name) {
#   require(ggplot2)
#   require(dplyr)
#   
#   res<-.self$get_resource(resource_name)
#   
#   plotset<-
#     res$monitor$data%>%
#     group_by(t) %>%
#     summarise(v = max(v))
#   
#   ggplot(plotset)+
#     aes(x=t, y=v) +
#     geom_step()+
#     geom_hline(y=res$capacity, lty=2, color="red") +
#     ggtitle(paste("Resource usage:", res$name)) +
#     scale_y_continuous(breaks=seq(0,1000,1)) +
#     ylab("in use") +
#     xlab("time")
#   
# })
# 
# Simulator$methods(stop_event = function(evt){
#   evt_filter<-
#     unlist(lapply(.self$events, function(obj) {
#       !identical(obj, evt)
#     }))
#   
#   .self$release_resources(evt)
#   
#   events <<- .self$events[evt_filter]
#   
#   # register stop of event for entity
#   .self$entities[[evt$entity_index]]$time_value_monitor$record(now(), 0)
#   
# }                 
# )
# 
# 
# 
# 
# Simulator$methods(simmer = function(until=Inf){  
#   
#   # create first event for all entities
#   for(ent_index in 1:length(.self$entities)){
#     .self$create_next_event(ent_index)
#   }
#   
#   
#   ## loop over event list
#   
#   while(.self$now() < until && length(.self$events)!=0){
#     
#     for(evt in .self$events){
#       
#       if(.self$now() >= evt$end_time){ ## event has ended, start next event
#         
#         
#         
#         new_evt<-.self$create_next_event(evt$entity_index)  ## also deletes finished event from event list and starts new event if possible
#         
#       } else if (.self$now() >= evt$early_start && !evt$has_started()){ ## event is waiting to start: check if event can start
#         .self$start_event(evt)
#         
#       }
#     }
#     
#     
#     
#     if(getOption("verbose")) message(paste("current time:", .self$now()))
#     
#     .self$goto_next_step()    
#     
#   }
#   
#   .self$stop_running_events()  
#   
#   return(.self)
# })
# 
# # 
# Simulator$methods(stop_running_events = function(){
#   for(evt in .self$events){
#     entities[[evt$entity_index]]$time_value_monitor$record(now(), NA)
#   }
# })
# 
# setMethod("show", "Simulator",
#           function(object) cat(paste("Simulator object",
#                                      "\n----------------",
#                                      "\nname:", object$name,
#                                      "\n# entities:",length(object$entities),
#                                      "\n# resources:", length(object$resources),
#                                      "\n# trajectories:", length(object$trajectories),
#                                      "\ntime:", object$now(),
#                                      "\n# events remaining:", length(object$events)))
# )
# 
# #' @export
# plot_resource_usage<-function(sim_obj, resource_name, ...){
#   sim_obj$plot_resource_usage(resource_name, ...)
# }
