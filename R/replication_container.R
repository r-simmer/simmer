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

ReplicationContainer$methods(plot_resource_usage = function(resource_name, ...){
  require(ggplot2)
  require(dplyr)
  
  if(length(list(...))>0){
    return(
      simulators[[list(...)[[1]]]]$plot_resource_usage(resource_name)
    )
  }
  
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
    geom_step(aes(group=rep), alpha=.2)+
    stat_smooth()+
    geom_hline(y=res$capacity, lty=2, color="red") +
    ggtitle(paste("Resource usage:", res$name)) +
    scale_y_continuous(breaks=seq(0,1000,1)) +
    ylab("in use") +
    xlab("time")
})

ReplicationContainer$methods(plot_resource_utilization = function(resource_name){
  require(ggplot2)
  require(scales)
  require(dplyr)
  
  
  run_times<-sapply(simulators, function(sim_obj) sim_obj$now())
  resources_names<-sapply(simulators[[1]]$resources, function(obj) obj$name)
  resources_capacity<-sapply(simulators[[1]]$resources, function(obj) obj$capacity)
  
  
  dataset<-
    do.call(rbind,
            mapply(function(sim_obj, rep, runtime) { 
              dataset<-
                do.call(rbind,
                        mapply(function(resource_name, resource_capacity){
                          dataset<- sim_obj$get_resource(resource_name)$monitor$data
                          dataset$resource<-resource_name
                          dataset$capacity<-resource_capacity
                          dataset
                        }, resources_names, resources_capacity, SIMPLIFY=F))  
              
              dataset$rep<-rep
              dataset$runtime<-runtime
              dataset
            }, simulators, 1:length(simulators),run_times, SIMPLIFY=F)
    )
  
  dataset<-
    dataset %>%
    group_by(resource, rep, capacity, runtime, t) %>%
    summarise(v=max(v)) %>%
    ungroup() %>%
    group_by(resource, rep, capacity, runtime) %>%
    mutate(in_use = (t-lag(t)) * lag(v)) %>%
    group_by(resource, rep, capacity, runtime) %>%
    summarise(in_use = sum(in_use, na.rm=T)) %>%
    ungroup() %>%
    mutate(utilization = in_use / capacity / runtime) %>%
    group_by(resource, capacity) %>%
    summarise(Q25 = quantile(utilization, .25),
              Q50 = quantile(utilization, .5),
              Q75 = quantile(utilization, .75))
  
  
  
  
  
  ggplot(dataset) +
    aes(x=resource, y=Q50, ymin=Q25, ymax=Q75) + 
    geom_bar(stat="identity") + 
    geom_errorbar(width=.25, color="black") +
    ggtitle("Resource utilization") +
    scale_y_continuous(labels=percent, limits=c(0,1), breaks=seq(0,2,.2)) +
    ylab("utilization")
})


ReplicationContainer$methods(plot_evolution_entity_times = function(type=c("flow_time","activity_time","waiting_time")){
  require(dplyr)
  #   simulators<-sim$simulators
  dataset<-
    do.call(rbind, 
            mapply(function(sim_obj, rep){
              do.call(rbind,
                      lapply(sim_obj$entities, function(ent){
                        
                        entity_data<-ent$time_value_monitor$data
                        
                        activity_data<-
                          entity_data %>%
                          group_by(t) %>%
                          summarise(v=max(v)) %>%
                          mutate(activity_time = (t-lag(t)) * lag(v)) %>%
                          ungroup() %>%
                          summarise(activity_time = sum(activity_time, na.rm=T)) %>%
                          data.frame(activity_time = ., 
                                     start_time = min(subset(entity_data, v>0, select="t")),
                                     end_time = entity_data[nrow(entity_data),"t"]) %>%
                          mutate(flow_time = end_time - start_time,
                                 waiting_time = flow_time - activity_time,
                                 replication = rep)
                        
                        activity_data
                        
                      })
              )
            }, simulators, 1:length(simulators), SIMPLIFY=F)
    ) %>%
    arrange(replication, flow_time)
  
  if(type=="flow_time"){
    ggplot(dataset) +
    aes(x=end_time, y=flow_time) +
    geom_line(alpha=.4, aes(group=replication)) +
    stat_smooth() +
    xlab("simulation time") +
    ylab("flow time") +
    ggtitle("Flow time evolution")
  } else if(type=="waiting_time"){
    ggplot(dataset) +
    aes(x=end_time, y=waiting_time) +
    geom_line(alpha=.4, aes(group=replication)) +
    stat_smooth() +
    xlab("simulation time") +
    ylab("waiting time") +
    ggtitle("Waiting time evolution")
  } else if(type=="activity_time"){
    ggplot(dataset) +
    aes(x=end_time, y=activity_time) +
    geom_line(alpha=.4, aes(group=replication)) +
    stat_smooth() +
    xlab("simulation time") +
    ylab("activity time") +
    ggtitle("Activity time evolution")
  }
  
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


#' @export
plot_evolution_entity_times<-function(replication_obj, ...){
  replication_obj$plot_evolution_entity_times(...)
}


#' @export
plot_resource_usage<-function(replication_obj, ...){
  replication_obj$plot_resource_usage(...)
}

#' @export
plot_resource_utilization<-function(replication_obj, ...){
  replication_obj$plot_resource_utilization(...)
}


