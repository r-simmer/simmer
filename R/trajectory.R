setClass("Trajectory", representation(name="character",
                                      events = "list"))

setMethod("initialize", "Trajectory", function(.Object, name) {
  .Object@name <- name
  .Object
})

setMethod("show", "Trajectory", function(object) {
  cat(paste0("Trajectory\nName: ", 
             object@name,
             "\n# events: ",
             length(object@events)))
})

#' create a trajectory object
#' 
#' @param name the name of the trajectory
#' @export
create_trajectory<-function(name){
  new("Trajectory", name)
}


#' add a seize event to the trajectory object
#' 
#' @param trajectory_obj the trajectory object
#' @param resource the resource as a character string
#' @param amount the amount of resources to seize
#' @export
add_seize_event<-function(trajectory_obj, resource, amount){
  trajectory_obj@events[[length(trajectory_obj@events) + 1]]<-c(list(type = "SeizeEvent",
                                                                     resource = resource,
                                                                     amount = amount))
  
  trajectory_obj
}

#' add a release event to the trajectory object
#' 
#' @param trajectory_obj the trajectory object
#' @param resource the resource as a character string
#' @param amount the amount of resources to release
#' @export
add_release_event<-function(trajectory_obj, resource, amount){
  trajectory_obj@events[[length(trajectory_obj@events) + 1]]<-c(list(type = "ReleaseEvent",
                                                                     resource = resource,
                                                                     amount = amount))
  
  trajectory_obj
}

#' add a time-out event to the trajectory object
#' 
#' @param trajectory_obj the trajectory object
#' @param duration the duration of the time-out event
#' @export
add_timeout_event<-function(trajectory_obj, duration){
  trajectory_obj@events[[length(trajectory_obj@events) + 1]]<-c(list(type = "TimeoutEvent",
                                                                     duration = duration))
  
  trajectory_obj
}

#' add a skip event to the trajectory object
#' 
#' @param trajectory_obj the trajectory object
#' @param number_to_skip the number of events to skip following the skip event, will be evaluated seperately for each entity if a character is given (e.g. "sample(c(0,1),1)")
#' @export
add_skip_event<-function(trajectory_obj, number_to_skip){
  trajectory_obj@events[[length(trajectory_obj@events) + 1]]<-c(list(type = "SkipEvent",
                                                                     number = number_to_skip))
  
  trajectory_obj
}



