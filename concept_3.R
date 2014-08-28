## TODOs 
## resource seizing blokkerend maken - DONE
## multiple resource seizing mogelijk maken - DONE
## simmer functie --> moet method van Simulator worden
## next_step beter/goed bepalen
## plot functies toevoegen
## simcontainer toevoegen : replications mogelijk maken + variations


setClass("trajectory", representation(name = "character", trajectory = "data.frame"))





Monitor <- setRefClass("Monitor",
                       fields = list(
                         data = "data.frame")
)


Monitor$methods(initialize = function(...){
  callSuper(...)
  .self$data <- data.frame(t=0, v=0)
  .self
})

Monitor$methods(record = function(t, v){
  data<<-rbind(data, data.frame(t, v))
})

Monitor$methods(get_last_value = function(t, v){
  return(data[nrow(data),"v"])
})


# TODO: vector of resourcerequirements toevoegen
Event <- setRefClass("Event", fields = list(description = "character",
                                            event_id = "character",
                                            entity_name = "character",
                                            entity_index = "numeric",
                                            early_start = "numeric",
                                            start_time = "numeric",
                                            end_time = "numeric",
                                            required_resources ="list",
                                            amount = "numeric",
                                            successor = "character",
                                            duration = "numeric"))




## Event init method (inits monitor data.frame)
Event$methods(initialize = function(...){
  callSuper(...)
  .self$end_time <- Inf
  .self
  
})

Event$methods(has_started = function(){
  if(length(.self$start_time)>0){
    return(TRUE)
  } else {
    return(FALSE)
  }
})


Entity <- setRefClass("Entity", 
                      fields = list(
                        name = "character",
                        entity_index = "numeric",
                        trajectory = "character",
                        trajectory_index = "numeric",
                        early_start_time="numeric",
                        current_event = "Event",
                        monitor = "Monitor")
)








### SIMULATOR CLASS

Simulator<-setRefClass("Simulator",
                       fields = list(
                         name = "character",
                         verbose = "logical",
                         entities = "vector",
                         current_time = "numeric",
                         resources = "vector",
                         trajectories = "vector",
                         events = "vector"))

Simulator$methods(initialize = function(...){
  callSuper(...)
  .self$verbose <- FALSE
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



Simulator$methods(goto_time = function(t) current_time <<- t)

Simulator$methods(add_entity = function(name, trajectory, start_time=0){
  
  new_entity<-
    Entity(name=name, 
           early_start_time=start_time, 
           trajectory=trajectory, 
           trajectory_index = .self$get_trajectory_index(trajectory), 
           entity_index = length(entities)+1)
  entities<<-push(entities, 
                  new_entity)
})

Simulator$methods(add_trajectory = function(name, trajectory){
  trajectories<<-push(trajectories,
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

# Simulator$methods(check_resource_availability = function(event_data){
#   #   print(event_data[1,"resource"])
#   resources<-unlist(strsplit(as.character(event_data[1,"resource"]),"/"))
#   amount_req<-as.numeric(unlist(strsplit(as.character(event_data[1,"amount"]),"/")))
#   #   print(amounts)
#   
#   result = c()
#   #   print(resources)
#   for(i in 1:length(resources)){
#     #     print(resources[i])
#     res<-.self$get_resource(resources[i])
#     #     print(tail(res$monitor,1))
#     if(tail(res$monitor,1)$in_use + amount_req[i] <= res$amount){
#       #       print(3)
#       result <- push(result, TRUE)
#     } else {
#       result <- push(result, FALSE)
#     }
#     
#   }
#   
#   return(result)
#   
# })

Simulator$methods(is_verbose = function(v){
  if(!v %in% c(TRUE, FALSE)) error("Valid options are TRUE or FALSE")
  
  verbose <<- v
  
})

Simulator$methods(create_next_event = function(entity_index){ 
  next_event <- .self$get_next_event_data(entity_index)
  entity <- .self$entities[[entity_index]]
  
  #   print(next_event)
  
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
    #     , mode = "any")
    
    
    new_evt = Event(event_id=as.character(next_event$event_id), 
                    entity_name = entity$name,
                    entity_index = entity_index,
                    description=paste0(as.character(next_event$description), "__", entity$name), 
                    required_resources = res_reqs, 
                    #                     amount=as.numeric(as.character(next_event$amount)),  # niet meer nodig?
                    duration=duration_evaluated,
                    early_start = .self$now() + entity$early_start_time,
                    successor=successor_evaluated)
    
    events <<- push(events, new_evt)
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

Simulator$methods(next_step = function (){
  if(length(.self$events)>0){
    min(
      sapply(.self$events, function(obj){
        if(obj$has_started()) {
          return(obj$end_time)
        } else {
          return(max(obj$early_start, now() + 1))
        }
      }
      )
    )
  } else {
    now()
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


Simulator$methods(plot_resource_usage = function(res_name) {
  require(ggplot2)
  require(dplyr)
  
  res<-.self$get_resource(res_name)
  
  plotset<-
    res$monitor$data%>%
    group_by(t) %>%
    summarise(v = max(v))
  
  ggplot(plotset)+
    aes(x=t, y=v) +
    geom_line()+
    geom_point() + 
    geom_hline(y=res$capacity, lty=2, color="red") +
    ggtitle(paste("Resource usage:", res$name))
  
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



Resource<-setRefClass("Resource", 
                      fields = list(
                        name = "character",
                        capacity = "numeric",
                        monitor = "Monitor"),
                      #                       methods = list(
                      #                         check_availability = function(amount_requested = 1) 0
                      #                       )
)

# ## Resource init method (inits monitor data.frame)
# Resource$methods(initialize = function(...){
#   callSuper(...)
#   .self$monitor <- data.frame(t=0,in_use=0)
#   .self
#   
# })



ResourceRequirement <- setRefClass("ResourceRequirement", field = list(name = "character",
                                                                       amount = "numeric",
                                                                       fulfilled = "logical",
                                                                       resource_obj = "Resource"))
ResourceRequirement$methods(initialize = function(...){
  callSuper(...)
  .self$amount <- 1
  .self$fulfilled <- FALSE
})



# pop function
pop<-function(v){
  v[1:length(v)-1]
}

# push function
push<-function(v, obj){
  c(v, obj)
  
}

BIG_M = exp(999)


create_simulator<-function(name = "anonymous"){
  Simulator$new(name=name, current_time=0)
}

add_resource<-function(sim_obj, name, capacity=1){
  sim_obj$resources<-push(sim_obj$resources,
                          Resource$new(name=name, capacity=capacity)
  )
  return(sim_obj)
}









add_entities_with_interval<-function(sim_obj, n, name_prefix, trajectory, interval, start_time=0){
  time <- start_time
  
  for(x in 1:n){
    sim_obj$add_entity(paste0(name_prefix,x), start_time=time, trajectory=trajectory)    
    time <- time + eval(substitute(interval))
  }
  return(sim_obj)
  
}



# filter a vector/list of S4 objects based on slot and the slot's value, return those matching
get_objects_by_filter<-function(vector_obj, slot, filter, sep="@"){
  #   print(vector_obj)
  results<-vector()
  for(obj in vector_obj){
    if(eval(parse(text=paste0("obj",sep,slot))) == filter){
      results<-push(results,obj)
    }
  }
  return(results)
  
}

get_objects_by_NOTfilter<-function(vector_obj, slot, filter, sep="@"){
  results<-vector()
  for(obj in vector_obj){
    if(eval(parse(text=paste0("obj",sep,slot))) != filter){
      results<-push(results,obj)
    }
  }
  return(results)
  
}

order_objects_by_slot_value<-function(vector_obj, slot){
  values <-
    do.call(c,
            lapply(vector_obj, function(obj) eval(parse(text=paste("obj$",slot)))
            )
    )
  vector_obj[order(values)]
}



add_trajectory<-function(sim_obj, name, trajectory){
  sim_obj$add_trajectory(name=name, trajectory=trajectory)
  return(sim_obj)
}




simmer <- function(sim_obj, until=Inf, verbose = FALSE){
  # set verbosity
  sim_obj$is_verbose(verbose)
  
  
  # create first event for all entities
  for(ent_index in 1:length(sim_obj$entities)){
    sim_obj$create_next_event(ent_index)
  }
  
  
  ## loop over event list
  
  while(sim_obj$now() < until && length(sim_obj$events)!=0){
    
    for(evt in sim_obj$events){
      
      if(sim_obj$now() >= evt$end_time){ ## event has ended, start next event
        
        
        
        new_evt<-sim_obj$create_next_event(evt$entity_index)  ## also deletes finished event from event list and starts new event if possible
        
      } else if (sim_obj$now() >= evt$early_start && !evt$has_started()){ ## event is waiting to start: check if event can start
        sim_obj$start_event(evt)
        
      }
    }

    
    
    if(verbose) message(paste("current time:", sim_obj$now()))
    #     sim_obj$goto_time(sim_obj$now()+1)    
    sim_obj$goto_time(sim_obj$next_step())    
    
  }
  
  
  
  return(sim_obj)
}


# sim2<-

# order_objects_by_slot_value(sim2@events, "early_start")


traj1<-
  read.table(header=T, text=
               "event_id description resource amount duration successor
                1 vpk vpk 1 rnorm(1,10) sample(c(2,3),1)
                2 arts arts 1 rnorm(1,10) NA
                3 logistieke logistieke 1 rnorm(1,10) NA"
  )

t2<-
  read.table(header=T, text=
               "event_id description resource amount duration successor
             1 vpk vpk/logistieke 1/1 rnorm(1,10) 2
             2 arts arts 1 rnorm(1,10) NA
             3 logistieke logistieke 1 rnorm(1,10) NA"
  )

library(magrittr)
sim<-
  create_simulator(name = "SuperDuperSim") %>%
  #   add_entity("test","r4e5rea4") 
  add_resource("vpk", 1) %>%
  add_resource("logistieke", 2) %>%
  add_resource("arts", 2) %>%
  add_trajectory("t1",t2) %>%
  add_entities_with_interval(10, "test", "t1", 5) 

# %>%
#   simmer()

simmer(sim, until = 240, verbose = TRUE)

