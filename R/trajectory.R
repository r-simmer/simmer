setClass("Trajectory", representation(name = "character", 
                                      timeline = "matrix"))

setMethod("initialize", "Trajectory", function(.Object, name){
  .Object@timeline <- matrix(nrow=0, ncol=4)
  colnames(.Object@timeline) <- c("event_id", "event_type", "value", "successor_id")
  
  .Object@name <- name  
  .Object
})

setMethod("show", "Trajectory", function(object){
  print(object@timeline, quote=F)
})



#' @export
create_trajectory<-function(name = "anonymous"){
  new("Trajectory", name = name)
}




#' add an event to a trajectory
#' 
#'@param event_type possible event types are \emph{seize}, \emph{release} and \emph{timeout}
#'@param resource_name (optional) used for specifying resource to be seized/released
#'@param resource_amount (optional) used for specifying amount of resource to be seized/released
#'@param time_value (optional) used for specifying timeout duration
#'@param event_id (optional) used to specify successor events, will auto generate if not specified, has to be unique
#'@param successor_id (optional) used to specify the successor of the current event, will automatically select next in line if not specified, \emph{"STOP"} means stop of trajectory
add_event<-function(trajectory_obj, event_type, ...){
  optional_args <- list(...)
  event_id <- ifelse("event_id" %in% names(optional_args), optional_args$event_id, NROW(trajectory_obj@timeline)+1)
  successor_id <- ifelse("successor_id" %in% names(optional_args), optional_args$successor_id, NROW(trajectory_obj@timeline)+2)
  
  
  if(event_type %in% c("seize", "release")){
    
    if(!"resource_amount" %in% names(optional_args)) stop("Please specify number of resources to be released/seized using the 'resource_amount=' parameter")
    if(!"resource_name" %in% names(optional_args)) stop("Please specify the resource type to be released/seized using the 'resource_name=' parameter")
    
    trajectory_obj@timeline<-rbind(
      trajectory_obj@timeline,
      c(event_id, event_type, optional_args$resource_amount, successor_id)
    )
  } else if(event_type %in% c("timeout")){
    
    if(!"time_value" %in% names(optional_args)) stop("Please specify a timeout duration using the 'time_value=' parameter")
    
    trajectory_obj@timeline<-rbind(
      trajectory_obj@timeline,
      c(event_id, event_type, optional_args$time_value, successor_id)
    )
  }
  
  
  if(getOption("verbose")) message(paste0("Added new event with event_id: ",event_id))
  
  trajectory_obj
  
}

#' seize n amount of resource x
#' 
#' @param trajectory_obj the trajectory object
#' @param resource_name the name of the resource
#' @param amount the amount of \emph{resource_name} to be seized
#' @export
seize_resource<-function(trajectory_obj, resource_name, amount, ...){
  trajectory_obj<-
    add_event(trajectory_obj, "seize", resource_name = resource_name, resource_amount = amount, ...)
  
  trajectory_obj
}

#' release n amount of resource x
#' 
#' @param trajectory_obj the trajectory object
#' @param resource_name the name of the resource
#' @param amount the amount of \emph{resource_name} to be seized
#' @export
release_resource<-function(trajectory_obj, resource_name, amount, ...){
  trajectory_obj<-
    add_event(trajectory_obj, "release", resource_name = resource_name, resource_amount = amount, ...)
  
  trajectory_obj
}

#' timeout entity for x duration
#' 
#' @param trajectory_obj the trajectory object
#' @param duration timeout x time units
#' @export
timeout_entity <- function(trajectory_obj, duration, ...){
  
  optional_args <- list(...)
  
  trajectory_obj<-
    add_event(trajectory_obj, "timeout", time_value = duration, ...)
  
  trajectory_obj
}