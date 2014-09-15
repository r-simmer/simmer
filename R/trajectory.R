setClass("Trajectory", representation(name = "character", 
                                      timeline = "matrix"))

setMethod("initialize", "Trajectory", function(.Object, name="anonymous"){
  .Object@timeline <- matrix(nrow=0, ncol=3)
  colnames(.Object@timeline) <- c("event_id", "event_type", "value")
  
  .Object@name <- name  
  .Object
})


#' @export
create_trajectory<-function(name){
  new("Trajectory", name = name)
}


#' add an event to a trajectory
#' 
#'@param event_type possible event types are \emph{seize}, \emph{release} and \emph{timeout}
#'@param resource_name (optional) used for specifying resource to be seized/released
#'@param resource_amount (optional) used for specifying amount of resource to be seized/released
#'@param time_value (optional) used for specifying timeout duration
#'@param event_id (optional) used to specify successor events, will auto generate if not specified, has to be unique
#'@export
add_event<-function(trajectory_obj, event_type, ...){
  optional_args <- list(...)
  event_id <- ifelse("event_id" %in% optional_args, optional_args$event_id, NROW(trajectory_obj@timeline)+1)
  
  if(event_type %in% c("seize", "release")){
    
    if(!"resource_amount" %in% names(optional_args)) stop("Please specify number of resources to be released/seized using the 'resource_amount=' parameter")
    if(!"resource_name" %in% names(optional_args)) stop("Please specify the resource type to be released/seized using the 'resource_name=' parameter")
    
    trajectory_obj@timeline<-rbind(
      trajectory_obj@timeline,
      c(event_id, event_type, optional_args$resource_amount)
    )
  } 
  
  trajectory_obj
  
}



# library(magrittr)
# create_trajectory("test") %>%
#   add_event("seize", resource_name="vpk", resource_amount=2)
