ReplicationContainer<-setRefClass("ReplicationContainer", fields=list(
  simulators = "vector"))

ReplicationContainer$methods(build_container = function(sim_obj, n_replications = 1){
  # TODO; list/vectors binnen refClasses moeten apart gekopieerd worden
  simulators<<-sapply(1:n_replications, function(x) deep_copy_simulator(sim_obj))
  return(.self)
})


ReplicationContainer$methods(simmer = function(until = BIG_M){
  for(sim in .self$simulators){
    sim$simmer(until)
  }
  
  return(.self)
})


#' @export
replicator<-function(sim_obj, n_replications){
  replication_container<-ReplicationContainer()
  replication_container$build_container(sim_obj, n_replications)
}


deep_copy_simulator<-function(sim_obj){
  obj_copy <- sim_obj$copy()
  obj_copy$resources <- sapply(obj_copy$resources, function(obj) obj$copy())
  obj_copy$events <- sapply(obj_copy$events, function(obj){
    evt_copy <- obj$copy()
    evt_copy$required_resources <- sapply(evt_copy$required_resources, function(req_res) req_res$copy())
  }
  )
  
  obj_copy$entities<-sapply(obj_copy$entities, function(obj) obj$copy())
  
  
  obj_copy
}




