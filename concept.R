

setClass("trajectory", representation(name = "character", trajectory = "data.frame"))



# 
# setMethod("show", "resource",
#           function(object) cat(paste0(object@name,"(",object@amount,")"))
# )

Entity <- setRefClass("Entity", 
                      fields = list(
                        name = "character",
                        trajectory = "character",
                        start_time="numeric",
                        current_step = "numeric")
)





setClass("event", representation(description = "character",
                                 event_id = "character",
                                 early_start = "numeric",
                                 start_time = "numeric",
                                 entity="Entity",
                                 resource="character",
                                 amount = "numeric",
                                 successor = "character",
                                 duration = "numeric"))

setMethod("as.character", "event",
          function(x, ...) paste(x@description,'|',x@event_id)
)


Simulator<-setRefClass("Simulator",
                       fields = list(
                         name = "character",
                         entities = "vector",
                         current_time = "numeric",
                         max_time = "numeric",
                         resources = "vector",
                         trajectories = "vector",
                         events = "vector"),
                       
                       methods = list(
                         
                         add_entity = function(name, trajectory, start_time=0){
                           entities<<-push(entities, 
                                           Entity$new(name=name, start_time=start_time, trajectory=trajectory))
                         },
                         
                         add_trajectory = function(name, trajectory){
                           trajectories<<-push(trajectories,
                                               new("trajectory", name=name, trajectory=trajectory)
                           )
                         },
                         
                         get_trajectory = function(name){
                           get_objects_by_filter(trajectories, "name", name)
                         },
                         
                         create_event = function(entity, event_id, description, resource, amount, duration, early_start, successor){
                           
                           events<<-push(events, 
                                         new("event", 
                                             entity=entity, 
                                             event_id=event_id, 
                                             description=description, 
                                             resource=resource, 
                                             amount=amount, 
                                             duration=duration,
                                             early_start=early_start,
                                             successor=successor)
                           )
                           order_events()
                         },
                         
                         order_events = function() {
                           events <<- order_objects_by_slot_value(events, "early_start")
                         },
                         
                         start_next_event = function(event) {
                           print(event@entity$trajectory)
                           
                         }
                       )
)

get_next_event_data <- function(sim, event){
  trajectory <- get_objects_by_filter(vector_obj = sim$trajectories,slot = "name",filter = event@entity$trajectory)@trajectory
  
  next_event_data <- trajectory[trajectory$event_id == event@event_id,]

  
}

Resource<-setRefClass("Resource", 
                      fields = list(
                        name = "character",
                        amount = "numeric",
                        monitor = "data.frame"),
                      methods = list(
                        check_availability = function(amount_requested = 1) 0
                      )
)

# setMethod("generate_events", "Simulator", function(object){
#   for(entity in object@entities){
#     print(3)
#   }  
# })

# setMethod("show", "Simulator",
#           function(object) cat(paste("Simulator object",
#                                      "\n----------------",
#                                      "\nname:", object$name,
#                                      "\n# entities:",length(object$entities),
#                                      "\nmax time:", object$max_time,
#                                      "\n# resources:", length(object$resources),
#                                      "\n# trajectories:", length(object$trajectories)))
# )

# in-place pop
pop<-function(v){
  v[1:length(v)-1]
}

# in-place push
push<-function(v, obj){
  c(v, obj)
  
}

# setGeneric("add_entity", function(name, max_time = Inf) {
#   standardGeneric("create_simulator")
# })
# 
# setMethod("create_simulator", signature(object = "Person"), function(object, x) {
#   object@age * x
# })

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
    sim_obj$add_entity(paste0(name_prefix,n), start_time=time, trajectory=trajectory)    
    time <- time + eval(substitute(interval))
  }
  return(sim_obj)
  
}



# filter a vector/list of S4 objects based on slot and the slot's value, return those matching
get_objects_by_filter<-function(vector_obj, slot, filter){
  do.call(na.omit,
          lapply(vector_obj, function(obj){
            
            if(eval(parse(text=paste("obj@",slot))) == filter){
              #               print(obj)
              return(obj)
            } else return(NA) 
          }          
          )
  )}

order_objects_by_slot_value<-function(vector_obj, slot){
  values <-
    do.call(c,
            lapply(vector_obj, function(obj) eval(parse(text=paste("obj@",slot)))
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



check_resources_available<-function(sim_obj, resource_name, amount_required=1){
  resource<-get_objects_by_filter(sim_obj@resources, "name", resource_name)
  if(length(resource)>0){
    # check if resource available
    if(resource@amount >= amount_required & (length(resource@monitor) == 0 || resource@amount - resource@monitor[nrow(resource@monitor),"in_use"] >= amount_required)){
      message("resources available")
      return(TRUE)
    } else {
      message("resources NOT available")
      return(FALSE)
    } 
  } else stop("requested resource not available in system")
}

seize_resources<-function(sim_obj, resource_name, amount_required  = 1){
  resource<-get_objects_by_filter(sim_obj@resources, "name", resource_name)
  current_in_use <- ifelse(length(resource@monitor)>0, resource@monitor[nrow(resource@monitor),"in_use"], 0)
  print(resource@monitor)
  resource@monitor<-rbind(resource@monitor, data.frame(t=sim_obj@current_time, in_use=amount_required + current_in_use))
}

add_trajectory<-function(sim_obj, name, trajectory){
  sim_obj$add_trajectory(name=name, trajectory=trajectory)
  return(sim_obj)
}


simmer <- function(sim_obj){

  # create first event for all entities
  for(ent in sim_obj$entities){
    
    #     
    #     next_step = ifelse(length(ent@current_step)==0, 1, ent@current_step + 1)
    #     ent@current_step = 1
    #     
    ent$current_step <- 1

    trj <- sim_obj$get_trajectory(ent$trajectory)
    entity_step <- trj@trajectory[ent$current_step,]

    message(paste("starting activity:", ent$current_step))
    #     print(entity_trajectory@trajectory[next_step,])
    
    duration_evaluated = eval(parse(text=entity_step["duration"][[1]]))
    early_start = ifelse(sim_obj$current_time >= ent$start_time, sim_obj$current_time, ent$start_time)
    
    sim_obj$create_event(
      entity = ent, 
      event_id = as.character(entity_step["event_id"][[1]]), 
      description = as.character(entity_step["description"][[1]]), 
      resource = as.character(entity_step["resource"][[1]]), 
      amount = as.numeric(entity_step["amount"][[1]]), 
      duration = duration_evaluated,
      successor = as.character(entity_step["successor"][[1]]),
      early_start = early_start)
    
  }
  

  
#   # iterate over eventlist, check if one has finished
#   for(evt in sim_obj@events){
#     # check if evt still has to start & current_time >= early_start
#     if(length(evt@start_time)==0 & sim_obj@current_time >= evt@early_start){
#       print(paste(evt, "is eligible to start"))
#       if(check_resources_available(sim_obj, evt@resource, evt@amount)){
#         seize_resources(sim_obj, evt@resource, evt@amount)
#       }
#     }
#     
#   }
  
  
  return(sim_obj)
}


# sim2<-

# order_objects_by_slot_value(sim2@events, "early_start")


traj1<-
  read.table(header=T, text=
               "event_id description resource amount duration successor
1 vpk vpk 1 rnorm(1,10) 2
2 arts arts 1 rnorm(1,10) END"
  )

library(dplyr)
sim<-
  create_simulator() %>%
  #   add_entity("test","r4e5rea4") 
  add_resource("vpk", 2) %>%
  add_resource("arts", 2) %>%
  add_entities_with_interval(15, "test", "t1", 10) %>%
  add_trajectory("t1",traj1) 
# %>%
#   simmer()

simmer(sim)

