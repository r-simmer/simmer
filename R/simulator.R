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


#' Create a simulator object
#' 
#' @param name the name of the simulator (defaults to 'anonymous')
#' @param n the number of replications
#' @param until the maximum run time of the simulation
#' @param verbose show log messages
#' @export
create_simulator<-function(sim_name = "anonymous", n=1, until = Inf, verbose = FALSE){  
  new("Simulator", sim_name, n, until, verbose)
}


#' Run the simulation
#' 
#' @param sim_obj the simulation object
#' @export
simmer<-function(sim_obj){
  
  for(i in 1:length(sim_obj@simulators)){
    message(paste("Starting replication",i))
    run_(sim_obj@simulators[[i]])
    message(paste("Finished replication",i))
  }
  return(sim_obj)
}

#' Add a resource to the simulation object
#' 
#' @param sim_obj the simulation object
#' @param the name of the resource
#' @param the capacity of the resource
#' @export
add_resource<-function(sim_obj, name, capacity){
  for(sim_ptr in sim_obj@simulators) add_resource_(sim_ptr, name, capacity)
  
  return(sim_obj)
}


evaluate_value<-function(value){
  tryCatch(
{
  abs(eval(parse(text=value)))
}, 
error = function(err) value)
}