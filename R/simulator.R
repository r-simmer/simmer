
### SIMULATOR CLASS

Simulator<-setRefClass("Simulator",
                       fields = list(
                         name = "character",
                         entities = "vector",
                         current_time = "numeric",
                         resources = "vector",
                         trajectories = "vector",
                         events = "vector"))

Simulator$methods(initialize = function(...){
  callSuper(...)
  .self$events <- vector()
  .self
})

Simulator$methods(now = function() current_time)

Simulator$methods(seize_resources = function(evt){
  for(req in evt$required_resources){
    in_use <- req$resource_obj$monitor$get_last_value()
    if(in_use + req$amount <= req$resource_obj$capacity && req$fulfilled == FALSE){
      req$resource_obj$monitor$record(now(), in_use + req$amount)
      req$fulfilled<-TRUE
    }
  }
  # return whether all resource requirements are fullfilled or not
  all(
    sapply(evt$required_resources, function(req) req$fulfilled)
  )
  
})

Simulator$methods(release_resources = function(evt){
  for(req in evt$required_resources){
    in_use <- req$resource_obj$monitor$get_last_value()
    
    req$resource_obj$monitor$record(now(), in_use - req$amount)
    
  }
})





Simulator$methods(add_entity = function(name, trajectory, start_time=0){
  
  new_entity<-
    Entity(name=name, 
           early_start_time=start_time, 
           trajectory=trajectory, 
           trajectory_index = .self$get_trajectory_index(trajectory), 
           entity_index = length(entities)+1)
  entities<<-c(entities, 
               new_entity)
})

Simulator$methods(add_trajectory = function(name, trajectory){
  trajectories<<-c(trajectories,
                   new("trajectory", name=name, trajectory=trajectory)
  )
})

Simulator$methods(get_trajectory = function(name){
  get_objects_by_filter(trajectories, "name", name)[[1]]
})

Simulator$methods(get_trajectory_index = function(name){
  trajectory_names <-
    do.call(c, 
            lapply(.self$trajectories, function(obj) return(obj@name)
            ))
  match(name, trajectory_names)
  
})


Simulator$methods(get_resource = function(name){
  get_objects_by_filter(resources, "name", name, sep="$")[[1]]
})


Simulator$methods(order_events = function() {
  events <<- order_objects_by_slot_value(events, "early_start")
})



Simulator$methods(get_next_event_data = function(entity_index) {
  
  trajectory <- .self$trajectories[[.self$entities[[entity_index]]$trajectory_index]]@trajectory
  
  if(length(.self$entities[[entity_index]]$current_event$start_time)>0){ # there is already a step running / has runned
    successor_id <- trajectory[trajectory$event_id == .self$entities[[entity_index]]$current_event$event_id, "successor"]
    successor_id_evaluated<-
      as.character(eval(parse(text=as.character(successor_id))))
    #     print(successor_id_evaluated)
    if(is.na(successor_id_evaluated)) {
      return(NULL)
    } else {
      next_event_data <-  trajectory[trajectory$event_id == successor_id_evaluated, ]  
      return(next_event_data)
    }
  } else { # first event of entity still has to start
    
    next_event_data<-trajectory[1,]
  }
  return(next_event_data)
}
)


Simulator$methods(create_next_event = function(entity_index){ 
  next_event <- .self$get_next_event_data(entity_index)
  entity <- .self$entities[[entity_index]]
  
  if(!is.null(next_event)){ ## if FALSE, end of trajectory is reached
    duration_evaluated <-
      floor(eval(parse(text=as.character(next_event$duration[[1]]))))
    successor_evaluated <-
      as.character(eval(parse(text=as.character(next_event$successor[[1]]))))
    
    
    res <- unlist(strsplit(as.character(next_event$resource),"/"))
    amounts <- unlist(strsplit(as.character(next_event$amount),"/"))
    
    
    res_reqs <- 
      mapply(function(r,a){
        ResourceRequirement(name = r, amount = as.numeric(a), resource_obj = .self$get_resource(r))
      }, res, amounts, SIMPLIFY = T, USE.NAMES = F)
    
    
    
    new_evt = Event(event_id=as.character(next_event$event_id), 
                    entity_name = entity$name,
                    entity_index = entity_index,
                    description=paste0(as.character(next_event$description), "__", entity$name), 
                    required_resources = res_reqs, 
                    duration=duration_evaluated,
                    early_start = .self$now() + entity$early_start_time,
                    successor=successor_evaluated)
    
    events <<- c(events, new_evt)
    events <<- order_objects_by_slot_value(events, slot = "early_start")
    
    if(length(entity$current_event$early_start)>0){ # if current_event is not none
      # stop current event
      .self$stop_event(entity$current_event)
    }
    
    
    
    entity$current_event <- new_evt
    
    .self$start_event(new_evt)
    
    return(new_evt)  
  } else { # record stop of current event
    .self$stop_event(entity$current_event)
    
  }
  
  return(NULL)
  
})

Simulator$methods(goto_next_step = function (){
  if(length(.self$events)>0){
    current_time <<- min(
      sapply(.self$events, function(obj){
        if(obj$has_started()) {
          return(obj$end_time)
        } else {
          return(ifelse(obj$early_start>now(), obj$early_start, Inf))
        }
      }
      )
    )
  }
  
})

Simulator$methods(start_event = function(evt){ #rename naar start_event
  if(.self$now() >= evt$early_start){
    
    # if all required resources are seized, start
    if(.self$seize_resources(evt)){
      
      evt$start_time <- .self$now()
      evt$end_time <- evt$start_time + evt$duration
      .self$entities[[evt$entity_index]]$monitor$record(now(), 1) # record start of processing of event
      return(TRUE)
    }
  }
  return(FALSE)
})


Simulator$methods(plot_resource_usage = function(resource_name) {
  require(ggplot2)
  require(dplyr)
  
  res<-.self$get_resource(resource_name)
  
  plotset<-
    res$monitor$data%>%
    group_by(t) %>%
    summarise(v = max(v))
  
  ggplot(plotset)+
    aes(x=t, y=v) +
    geom_step()+
    geom_hline(y=res$capacity, lty=2, color="red") +
    ggtitle(paste("Resource usage:", res$name)) +
    scale_y_continuous(breaks=seq(0,1000,1)) +
    ylab("in use") +
    xlab("time")
  
})

Simulator$methods(stop_event = function(evt){
  evt_filter<-
    unlist(lapply(.self$events, function(obj) {
      !identical(obj, evt)
    }))
  
  .self$release_resources(evt)
  
  events <<- .self$events[evt_filter]
  
  # register stop of event for entity
  .self$entities[[evt$entity_index]]$monitor$record(now(), 0)
  
}                 
)




Simulator$methods(simmer = function(until=Inf){  
  
  # create first event for all entities
  for(ent_index in 1:length(.self$entities)){
    .self$create_next_event(ent_index)
  }
  
  
  ## loop over event list
  
  while(.self$now() < until && length(.self$events)!=0){
    
    for(evt in .self$events){
      
      if(.self$now() >= evt$end_time){ ## event has ended, start next event
        
        
        
        new_evt<-.self$create_next_event(evt$entity_index)  ## also deletes finished event from event list and starts new event if possible
        
      } else if (.self$now() >= evt$early_start && !evt$has_started()){ ## event is waiting to start: check if event can start
        .self$start_event(evt)
        
      }
    }
    
    
    
    if(getOption("verbose")) message(paste("current time:", .self$now()))
    
    .self$goto_next_step()    
    
  }
  
  
  
  return(.self)
})

setMethod("show", "Simulator",
          function(object) cat(paste("Simulator object",
                                     "\n----------------",
                                     "\nname:", object$name,
                                     "\n# entities:",length(object$entities),
                                     "\n# resources:", length(object$resources),
                                     "\n# trajectories:", length(object$trajectories),
                                     "\ntime:", object$now(),
                                     "\n# events remaining:", length(object$events)))
)

#' @export
plot_resource_usage<-function(sim_obj, resource_name, ...){
  sim_obj$plot_resource_usage(resource_name, ...)
}
