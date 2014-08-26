

setClass("trajectory", representation(name = "character", trajectory = "data.frame"))



Event <- setRefClass("Event", fields = list(description = "character",
                                            event_id = "character",
                                            early_start = "numeric",
                                            start_time = "numeric",
                                            end_time = "numeric",
                                            resource="character",
                                            amount = "numeric",
                                            successor = "character",
                                            duration = "numeric"))
## Event init method (inits monitor data.frame)
Event$methods(initialize = function(...){
  callSuper(...)
  .self$end_time <- Inf
  .self
  
})



Entity <- setRefClass("Entity", 
                      fields = list(
                        name = "character",
                        trajectory = "character",
                        early_start_time="numeric",
                        current_event = "Event")
)






setMethod("as.character", "event",
          function(x, ...) paste(x@description,'|',x@event_id)
)


### SIMULATOR CLASS

Simulator<-setRefClass("Simulator",
                       fields = list(
                         name = "character",
                         entities = "vector",
                         current_time = "numeric",
                         max_time = "numeric",
                         resources = "vector",
                         trajectories = "vector",
                         events = "vector"))

Simulator$methods(now = function() current_time)

Simulator$methods(goto_time = function(t) current_time <<- t)

Simulator$methods(add_entity = function(name, trajectory, start_time=0){
  entities<<-push(entities, 
                  Entity$new(name=name, early_start_time=start_time, trajectory=trajectory))
})

Simulator$methods(add_trajectory = function(name, trajectory){
  trajectories<<-push(trajectories,
                      new("trajectory", name=name, trajectory=trajectory)
  )
})

Simulator$methods(get_trajectory = function(name){
  get_objects_by_filter(trajectories, "name", name)[[1]]
})

Simulator$methods(get_resource = function(name){
  get_objects_by_filter(resources, "name", name, sep="$")[[1]]
})


Simulator$methods(order_events = function() {
  events <<- order_objects_by_slot_value(events, "early_start")
})

Simulator$methods(get_next_event_data = function(entity) {
#   print(entity)
  trajectory <- get_objects_by_filter(vector_obj = .self$trajectories,slot = "name",filter = entity$trajectory)[[1]]@trajectory
#   print(trajectory)
  if(length(entity$current_event$start_time)>0){ # there is already a step running / has runned
    successor_id <- trajectory[trajectory$event_id == entity$current_event$event_id, "successor"]
    if(successor_id == "END") {
      return(NULL)
    } else {
      next_event_data <-  trajectory[trajectory$event_id == successor_id, ]  
    }
  } else { # first event of entity still has to start
    next_event_data<-trajectory[1,]
  }
  return(next_event_data)
}
)

Simulator$methods(check_resource_availability = function(event_data){
  #   print(event_data[1,"resource"])
  resources<-unlist(strsplit(as.character(event_data[1,"resource"]),"/"))
  amount_req<-as.numeric(unlist(strsplit(as.character(event_data[1,"amount"]),"/")))
  #   print(amounts)
  
  result = c()
  #   print(resources)
  for(i in 1:length(resources)){
    #     print(resources[i])
    res<-.self$get_resource(resources[i])
    #     print(tail(res$monitor,1))
    if(tail(res$monitor,1)$in_use + amount_req[i] <= res$amount){
      #       print(3)
      result <- push(result, TRUE)
    } else {
      result <- push(result, FALSE)
    }
    
  }
  
  return(result)
  
})

Simulator$methods(create_next_event = function(entity){  ## wel direct aanmaken en in eventlist zetten, daarna pas kijken of ook gestart kan worden
  next_event = .self$get_next_event_data(entity)
  print(next_event)
  if(is.null(next_event)==FALSE){ ## if FALSE, end of trajectory is reached
    duration_evaluated <-
      floor(eval(parse(text=as.character(next_event$duration[[1]]))))
    successor_evaluated <-
      as.character(eval(parse(text=as.character(next_event$successor[[1]]))))
    
    new_evt = Event(event_id=as.character(next_event$event_id), 
                    description=paste0(as.character(next_event$description), "__", entity$name), 
                    resource=as.character(next_event$resource), 
                    amount=next_event$amount, 
                    duration=duration_evaluated,
                    early_start = .self$now() + entity$early_start_time,
                    successor=successor_evaluated)
    
    events <<- push(events, new_evt)
    if(length(entity$current_event$early_start)>0){
      events <<- order_objects_by_slot_value(events, slot = "early_start")
      events <<- get_objects_by_NOTfilter(events, "description", entity$current_event$description)
    }
    
    
    
    entity$current_event <- new_evt
    
    .self$start_if_possible(new_evt)
    
    
  }
  return(FALSE)
  
})

Simulator$methods(start_if_possible = function(evt){
  if(.self$now() >= evt$early_start){
    evt$start_time <- .self$now()
    evt$end_time <- evt$start_time + evt$duration
  }
})


setMethod("show", "Simulator",
          function(object) cat(paste("Simulator object",
                                     "\n----------------",
                                     "\nname:", object$name,
                                     "\n# entities:",length(object$entities),
                                     "\nmax time:", object$max_time,
                                     "\n# resources:", length(object$resources),
                                     "\n# trajectories:", length(object$trajectories),
                                     "\ntime:", object$now()))
)



Resource<-setRefClass("Resource", 
                      fields = list(
                        name = "character",
                        amount = "numeric",
                        monitor = "data.frame"),
                      methods = list(
                        check_availability = function(amount_requested = 1) 0
                      )
)

## Resource init method (inits monitor data.frame)
Resource$methods(initialize = function(...){
  callSuper(...)
  .self$monitor <- data.frame(t=0,in_use=0)
  .self
  
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


create_simulator<-function(name = "anonymous", max_time = BIG_M){
  Simulator$new(name=name, max_time=max_time, current_time=0)
}

add_resource<-function(sim_obj, name, amount=1){
  sim_obj$resources<-push(sim_obj$resources,
                          Resource$new(name=name, amount=amount)
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

# get_objects_by_filter(sim@trajectories, "name", "t1")

# parameter="name"
# filter="t1"
# for(x in sim@trajectories){
#   print(eval(parse(text=paste0("x@",parameter))))
#   print(eval(paste0("x@",parameter)) == filter)
# }



# check_resources_available<-function(sim_obj, resource_name, amount_required=1){
#   resource<-get_objects_by_filter(sim_obj@resources, "name", resource_name)[[1]]
#   if(length(resource)>0){
#     # check if resource available
#     if(resource@amount >= amount_required & (length(resource@monitor) == 0 || resource@amount - resource@monitor[nrow(resource@monitor),"in_use"] >= amount_required)){
#       message("resources available")
#       return(TRUE)
#     } else {
#       message("resources NOT available")
#       return(FALSE)
#     } 
#   } else stop("requested resource not available in system")
# }

# seize_resources<-function(sim_obj, resource_name, amount_required  = 1){
#   resource<-get_objects_by_filter(sim_obj@resources, "name", resource_name)[[1]]
#   current_in_use <- ifelse(length(resource@monitor)>0, resource@monitor[nrow(resource@monitor),"in_use"], 0)
#   print(resource@monitor)
#   resource@monitor<-rbind(resource@monitor, data.frame(t=sim_obj@current_time, in_use=amount_required + current_in_use))
# }

add_trajectory<-function(sim_obj, name, trajectory){
  sim_obj$add_trajectory(name=name, trajectory=trajectory)
  return(sim_obj)
}




simmer <- function(sim_obj, until=Inf){
  # create first event for all entities
  for(ent in sim_obj$entities){
    sim_obj$create_next_event(ent)
  }
  
  
  ## loop over event list
  while(sim_obj$now() < until){
    
    for(evt in sim_obj$events){
      
      if(sim_obj$now() >= evt$end_time){ ## event has ended, start next event
        sim_obj$create_next_event(evt$entity)  ## also deletes current event from event list and starts it if possible
      } else if (sim_obj$now() >= evt$early_start){ ## check if event can start
        sim_obj$start_if_possible(evt)
      }
    }
    
    

    sim_obj$goto_time(sim_obj$now()+1)    
   
    
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

library(dplyr)
sim<-
  create_simulator() %>%
  #   add_entity("test","r4e5rea4") 
  add_resource("vpk", 2) %>%
  add_resource("logistieke", 2) %>%
  add_resource("arts", 2) %>%
  add_entities_with_interval(15, "test", "t1", 5) %>%
  add_trajectory("t1",traj1) 
# %>%
#   simmer()

simmer(sim, 11)

