setClass("Trajectory", representation(name = "character", 
                                      timeline = "list"))

setMethod("show", "Trajectory", function(object){
  print(object@timeline)
})



#' @export
create_trajectory<-function(name = "anonymous"){
  new("Trajectory", name = name)
}




#' seize n amount of resource x
#' 
#' @param trajectory_obj the trajectory object
#' @param resource_name the name of the resource
#' @param amount the amount of \emph{resource_name} to be seized
#' @export
seize_resource<-function(trajectory_obj, resource_name, amount, description = "undefined", successor_id = NA, event_id = NA){
  event_id = ifelse(is.na(event_id), NROW(trajectory_obj@timeline) + 1, successor_id)
  succesor_id = ifelse(is.na(successor_id), event_id + 1, successor_id)
  
  trajectory_obj@timeline<-c(trajectory_obj@timeline, 
                             new("SeizeEvent",
                                 event_id = as.character(event_id),
                                 successor_id = as.character(successor_id),
                                 description = as.character(description), 
                                 resource_name = as.character(resource_name),
                                 resource_amount = amount
                             ))
  
  trajectory_obj
  
  trajectory_obj
}

#' release n amount of resource x
#' 
#' @param trajectory_obj the trajectory object
#' @param resource_name the name of the resource
#' @param amount the amount of \emph{resource_name} to be seized
#' @export
release_resource<-function(trajectory_obj, resource_name, amount, description = "undefined", successor_id = NA, event_id = NA){
  
  event_id = ifelse(is.na(event_id), NROW(trajectory_obj@timeline) + 1, successor_id)
  succesor_id = ifelse(is.na(successor_id), event_id + 1, successor_id)
  
  trajectory_obj@timeline<-c(trajectory_obj@timeline, 
                             new("ReleaseEvent",
                                 event_id = as.character(event_id),
                                 successor_id = as.character(successor_id),
                                 description = as.character(description), 
                                 resource_name = as.character(resource_name),
                                 resource_amount = amount
                             ))
  
  trajectory_obj
}

#' timeout entity for x duration
#' 
#' @param trajectory_obj the trajectory object
#' @param duration timeout x time units
#' @export
timeout_entity <- function(trajectory_obj, duration, description = "undefined", successor_id = NA, event_id = NA){
  
  trajectory_obj@timeline<-c(trajectory_obj@timeline, 
                             new("TimeoutEvent",
                                 event_id = as.character(event_id),
                                 successor_id = as.character(successor_id),
                                 description = description, 
                                 duration = duration
                             ))
  
  trajectory_obj
}