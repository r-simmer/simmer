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


#' @export
create_trajectory<-function(name){
  new("Trajectory", name)
}

#' @export
add_seize_event<-function(trajectory_obj, resource, amount){
  trajectory_obj@events[[length(trajectory_obj@events) + 1]]<-c(list(type = "SeizeEvent",
                                                     resource = resource,
                                                     amount = amount))
  
  trajectory_obj
}

#' @export
add_release_event<-function(trajectory_obj, resource, amount){
  trajectory_obj@events[[length(trajectory_obj@events) + 1]]<-c(list(type = "ReleaseEvent",
                                                       resource = resource,
                                                       amount = amount))
  
  trajectory_obj
}

#' @export
add_timeout_event<-function(trajectory_obj, duration){
  trajectory_obj@events[[length(trajectory_obj@events) + 1]]<-c(list(type = "TimeoutEvent",
                                                       duration = duration))
  
  trajectory_obj
}



