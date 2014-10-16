require(magrittr)

setClass("Simulator", representation(simulators = "vector",
                                     n = "numeric",
                                     until = "numeric",
                                     name = "character",
                                     verbose = "logical"))

setMethod( "initialize", "Simulator", function(.Object, name, n, until, verbose) {
  .Object@name <- name
  .Object@n <- n
  .Object@until <- until
  .Object@verbose <- verbose
  
  for(i in 1:n){
    if(is.finite(until)) sim <- Simulator__new(name, until, verbose)
    else sim <- Simulator__new(name, -1, verbose)
    .Object@simulators<-c(.Object@simulators, sim)
  }
  .Object
  
})

setMethod("show", "Simulator", function(object) {
  cat(paste0("Simulator object\nName: ", 
             object@name, 
             "\nUntil: ",
             object@until, 
             "\nVerbose: ",
             object@verbose,
             "\n# replications: ",
             object@n))
})


# 
# setClass("Simulator", representation(pointer = "externalptr",
#                                      until = "numeric",
#                                      name = "character",
#                                      verbose = "logical"))
# 
# setMethod( "initialize", "Simulator", function(.Object, name, until, verbose) {
#   .Object@name <- name
#   .Object@until <- until
#   .Object@verbose <- verbose
#   
#   if(is.finite(until)) .Object@pointer <- Simulator__new(name, until, verbose)
#   else .Object@pointer <- Simulator__new(name, -1, verbose)
#   .Object
# } )

# 
# setMethod("show", "Simulator", function(object) {
#   cat(paste0("Simulator object\nName: ", 
#              object@name, 
#              "\nUntil: ",
#              object@until, 
#              "\nVerbose: ",
#              object@verbose))
# })

# #' Creates a simulator object
# #' 
# #' @param sim_name the name of the simulator (defaults to 'anonymous')
# #' @param until the maximum run time of the simulation
# #' @param verbose show log messagses
# #' @export
# create_simulator<-function(sim_name = "anonymous", until = Inf, verbose = FALSE){  
#   new("Simulator", sim_name, until, verbose)
#   
# }


#' Creates a simulator object
#' 
#' @param name the name of the simulator (defaults to 'anonymous')
#' @param n the number of replications
#' @param until the maximum run time of the simulation
#' @param verbose show log messages
#' @export
create_simulator<-function(sim_name = "anonymous", n=1, until = Inf, verbose = FALSE){  
  new("Simulator", sim_name, n, until, verbose)
}

# #' @export
# add_entity<-function(sim_obj, entity_obj){
#   add_entity_(sim_obj@pointer, entity_obj@pointer)
#   return(sim_obj)
# }




#' @export
simmer<-function(sim_obj){
  
  for(i in 1:length(sim_obj@simulators)){
    message(paste("Starting replication",i))
    run_(sim_obj@simulators[[i]])
    message(paste("Finished replication",i))
  }
  return(sim_obj)
}

#' @export
add_resource<-function(sim_obj, name, capacity){
  for(sim_ptr in sim_obj@simulators) add_resource_(sim_ptr, name, capacity)
  
  return(sim_obj)
}

#' @export
get_entity_monitor_values<-function(sim_obj, aggregated = FALSE){
  
  dataset <- 
    do.call(rbind,
            lapply(1:length(sim_obj@simulators),function(i){
              monitor_data<-
                as.data.frame(
                  get_entity_monitor_values_(sim_obj@simulators[[i]])
                )
              monitor_data$replication<-i
              
              monitor_data
            }
            )
    )
  
  if(aggregated){
    require(dplyr)
    dataset<-
      dataset %>%
      group_by(replication, entity_id) %>%
      mutate(start_time = min(time[value==-999]),
             end_time = max(time[value==-999]),
             finished = ifelse(start_time == end_time, 0, 1),
             value_adj = ifelse(value<0, 0, value)) %>%
      group_by(replication, entity_id, time, start_time, end_time, finished) %>%
      summarise(value=max(value_adj)) %>%
      group_by(replication, entity_id, start_time, end_time, finished) %>%
      mutate(activity_time = (time-lag(time)) * lag(value)) %>%
      summarise(activity_time = sum(activity_time, na.rm=T)) %>%
      mutate(flow_time = end_time - start_time,
             waiting_time = flow_time - activity_time) %>%
      filter(finished == 1) %>%
      arrange(replication, end_time)
  }
  as.data.frame(dataset)
  
}

#' @export
get_resource_monitor_values<-function(sim_obj, resource_name){
  do.call(rbind,
          lapply(1:length(sim_obj@simulators),function(i){
            monitor_data<-
              as.data.frame(
                get_resource_monitor_values_(sim_obj@simulators[[i]], resource_name)
              )
            monitor_data$replication<-i
            monitor_data
          }
          )
  )
  
}

#' @export
evaluate_value<-function(value){
  tryCatch(
{
  abs(eval(parse(text=value)))
}, 
error = function(err) value)
}