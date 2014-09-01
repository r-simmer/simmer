
BIG_M = exp(999)

#' @export
create_simulator<-function(name = "anonymous"){
  Simulator$new(name=name, current_time=0)
}


#' @export
add_resource<-function(sim_obj, name, capacity=1){
  sim_obj$resources<-c(sim_obj$resources,
                          Resource$new(name=name, capacity=capacity)
  )
  return(sim_obj)
}


#' @export
add_entities_with_interval<-function(sim_obj, n, name_prefix, trajectory, interval, start_time=0){
  time <- start_time
  
  for(x in 1:n){
    sim_obj$add_entity(paste0(name_prefix,x), start_time=time, trajectory=trajectory)    
    time <- time + eval(substitute(interval))
  }
  return(sim_obj)
  
}




#' @export
order_objects_by_slot_value<-function(vector_obj, slot){
  values <-
    do.call(c,
            lapply(vector_obj, function(obj) eval(parse(text=paste("obj$",slot)))
            )
    )
  vector_obj[order(values)]
}


#' @export
add_trajectory<-function(sim_obj, name, trajectory){
  sim_obj$add_trajectory(name=name, trajectory=trajectory)
  return(sim_obj)
}

#' the simmer function
#' @param sim_obj input simboject
#' @export
simmer <- function(sim_obj, until=Inf){
  
  sim_obj$simmer(until = until)
}


