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

ReplicationContainer$methods(plot_resource_usage = function(resource_name){
  require(ggplot2)
  require(dplyr)
  
  res<-simulators[[1]]$get_resource(resource_name)
  
  dataset<-
  do.call(rbind,
          mapply(function(sim_obj, rep) { 
            dataset<- sim_obj$get_resource(resource_name)$monitor$data
            dataset$rep<-rep
            dataset
          }, simulators, 1:length(simulators), SIMPLIFY=F)
  ) %>%
    group_by(t, rep) %>%
    summarise(v = max(v))
  
  ggplot(dataset) +
    aes(x=t, y=v) + 
    geom_line(aes(group=rep))+
    stat_smooth()+
    geom_hline(y=res$capacity, lty=2, color="red") +
    ggtitle(paste("Resource usage:", res$name))
})
# 
# ReplicationContainer$methods(plot_resources_utilization = function(resource_name){
#   require(ggplot2)
#   require(dplyr)
#   
#   resources_names<-sapply(simulators[[1]]$resources, function(obj) obj$name)
#   
#   dataset<-
#     do.call(rbind,
#             mapply(function(sim_obj, rep) { 
#               dataset<- sim_obj$get_resource(resource_name)$monitor$data
#               dataset$rep<-rep
#               dataset
#             }, simulators, 1:length(simulators), resources_names, SIMPLIFY=F)
#     ) %>%
#     group_by(t, rep) %>%
#     summarise(v = max(v))
#   
#   ggplot(dataset) +
#     aes(x=t, y=v) + 
#     geom_line(aes(group=rep))+
#     stat_smooth()+
#     geom_hline(y=res$capacity, lty=2, color="red") +
#     ggtitle(paste("Resource usage:", res$name))
# })




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

#' @export
plot_resource_usage<-function(replication_obj, ...){
  replication_obj$plot_resource_usage(...)
}

