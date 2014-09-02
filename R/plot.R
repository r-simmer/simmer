#' plot utilization of a resource over time
#' 
#' plot the usage of a resource over the simulation time frame
#' @param sim_obj the simulation object
#' @param resource_name the name of the resource (character value)
#' @param replication specify to plot only a specific replication (default=FALSE)
#' @export
plot_resource_usage <- function(sim_obj, resource_name, replication=FALSE){
  
  require(ggplot2)
  require(dplyr)
  
  
  if(class(sim_obj)=="Simulator"){
    simulators<-c(sim_obj)
  } else simulators<-sim_obj$simulators
  
  if(!replication==F){
    simulators<-c(simulators[[replication]])
  }
  
  capacity<-simulators[[1]]$resources_capacity[[resource_name]]
  
  
  dataset<-
    do.call(rbind,
            mapply(function(sim_obj, rep) { 
              dataset<- sim_obj$resources_monitor[[resource_name]]$data
              dataset$rep<-rep
              dataset
            }, simulators, 1:length(simulators), SIMPLIFY=F)
    ) %>%
    group_by(t, rep) %>%
    summarise(v = max(v))
  

  plot_obj<-
  ggplot(dataset) +
    aes(x=t, y=v) + 
    geom_step(aes(group=rep), alpha=.4)+
    geom_hline(y=capacity, lty=2, color="red") +
    ggtitle(paste("Resource usage:", resource_name)) +
    scale_y_continuous(breaks=seq(0,1000,1)) +
    ylab("in use") +
    xlab("time") +
    expand_limits(y=0)
  
  if(replication==F){
    plot_obj +
      stat_smooth()      
  } else plot_obj
}

#' plot utilization of resources
#' 
#' plot the utilization of all resources in the simulation
#' @param sim_obj the simulation object
#' @export
plot_resource_utilization <- function(sim_obj){
  require(ggplot2)
  require(scales)
  require(dplyr)
  
  if(class(sim_obj)=="Simulator"){
    simulators<-c(sim_obj)
  } else simulators<-sim_obj$simulators
  
  
  run_times<-sapply(simulators, function(sim_obj) sim_obj$now())
  resources_names<-names(simulators[[1]]$resources_monitor)
  resources_capacity<-simulators[[1]]$resources_capacity
  
  
  dataset<-
    do.call(rbind,
            mapply(function(sim_obj, rep, runtime) { 
              dataset<-
                do.call(rbind,
                        mapply(function(resource_name, resource_capacity){
                          dataset<- sim_obj$resources_monitor[[resource_name]]$data
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
}

#' plot evolution of entity times
#' 
#' plot the evolution of entity related times (flow, activity and waiting time)
#' @param sim_obj the simulation object
#' @param type one of c("flow_time","activity_time","waiting_time")
#' @export
plot_evolution_entity_times <- function(sim_obj, type=c("flow_time","activity_time","waiting_time")){
  require(dplyr)
  require(ggplot2)
  
  if(class(sim_obj)=="Simulator"){
    simulators<-c(sim_obj)
  } else simulators<-sim_obj$simulators
  
  
  dataset<-
    do.call(rbind, 
            mapply(function(sim_obj, rep){
              do.call(rbind,
                      lapply(sim_obj$entities, function(ent){
                        
                        entity_data<-ent$time_value_monitor$data
                        
                        if(is.na(entity_data[nrow(entity_data),"v"])) {
                          return(data.frame())
                        } else {
                        
                        activity_data<-
                          entity_data %>%
                          group_by(t) %>%
                          summarise(v=max(v)) %>%
                          mutate(activity_time = (t-lag(t)) * lag(v)) %>%
                          ungroup() %>%
                          summarise(activity_time = sum(activity_time, na.rm=T)) %>%
                          data.frame(activity_time = ., 
                                     start_time = min(subset(entity_data, v>0, select="t")),
                                     end_time = entity_data[nrow(entity_data),"t"], entity=ent$name) %>%
                          mutate(flow_time = end_time - start_time,
                                 waiting_time = flow_time - activity_time,
                                 replication = rep)
                        return(activity_data)
                        }
                        
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
      ggtitle("Flow time evolution") +
      expand_limits(y=0)
  } else if(type=="waiting_time"){
    ggplot(dataset) +
      aes(x=end_time, y=waiting_time) +
      geom_line(alpha=.4, aes(group=replication)) +
      stat_smooth() +
      xlab("simulation time") +
      ylab("waiting time") +
      ggtitle("Waiting time evolution") +
      expand_limits(y=0)
  } else if(type=="activity_time"){
    ggplot(dataset) +
      aes(x=end_time, y=activity_time) +
      geom_line(alpha=.4, aes(group=replication)) +
      stat_smooth() +
      xlab("simulation time") +
      ylab("activity time") +
      ggtitle("Activity time evolution") +
      expand_limits(y=0)
  }
  
}