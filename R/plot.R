#' plot utilization of a resource over time
#' 
#' plot the usage of a resource over the simulation time frame
#' @param sim_obj the simulation object
#' @param resource_name the name of the resource (character value)
#' @param replication_n specify to plot only a specific replication (default=FALSE)
#' @param smooth_line adds a smoothline, usefull if a lot of different replications are plotted
#' @export
plot_resource_usage <- function(sim_obj, resource_name, replication_n=FALSE, smooth_line = FALSE){
  
  require(ggplot2)
  require(dplyr)
  
  monitor_data<-
    get_resource_serve_mon_values(sim_obj, resource_name)
  
  
  if(!replication_n==F){
    monitor_data <- monitor_data %>%
      filter(replication == replication_n)
  }
  
  capacity<-get_resource_capacity_(sim_obj@simulators[[1]], resource_name)
  
  monitor_data <-
    monitor_data %>%
    group_by(time, replication) %>%
    summarise(value = max(value))
  
  
  plot_obj<-
    ggplot(monitor_data) +
    aes(x=time, y=value) + 
    geom_step(aes(group=replication), alpha=.4) +
    geom_hline(y=capacity, lty=2, color="red") +
    ggtitle(paste("Resource usage:", resource_name)) +
    scale_y_continuous(breaks=seq(0,1000,1)) +
    ylab("in use") +
    xlab("time") +
    expand_limits(y=0)
  
  if(smooth_line == T){
    plot_obj +
      stat_smooth()      
  } else plot_obj
}

#' plot utilization of resources
#' 
#' plot the utilization of specified resources in the simulation
#' @param sim_obj the simulation object
#' @param resources a character vector with at least one resource specified - e.g. "c('res1','res2')"
#' @export
plot_resource_utilization <- function(sim_obj, resources){
  require(ggplot2)
  require(scales)
  require(dplyr)
  
  monitor_data<-
    do.call(rbind,
            lapply(resources, function(res) get_resource_serve_mon_values(sim_obj, res))
    ) %>%
    left_join(
      do.call(rbind,
              lapply(resources, function(res) data.frame(resource = res, capacity = get_resource_capacity_(sim_obj@simulators[[1]], res)))
      )
    ) %>%
    left_join(
      do.call(rbind,
      lapply(1:length(sim_obj@simulators), function(r) data.frame(replication = r, runtime = now_(sim_obj@simulators[[1]])))
      )
    ) %>%
    group_by(resource, replication, capacity, runtime, time) %>%
    summarise(value=max(value)) %>%
    group_by(resource, replication, capacity, runtime) %>%
    mutate(in_use = (time-lag(time)) * lag(value)) %>%
    group_by(resource, replication, capacity, runtime) %>%
    summarise(in_use = sum(in_use, na.rm=T)) %>%
    ungroup() %>%
    mutate(utilization = in_use / capacity / runtime) %>%
    group_by(resource, capacity) %>%
    summarise(Q25 = quantile(utilization, .25),
              Q50 = quantile(utilization, .5),
              Q75 = quantile(utilization, .75))
  

  
  ggplot(monitor_data) +
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
  
  monitor_data<-
    get_entity_monitor_values(sim_obj, aggregated = T)
  
  if(type=="flow_time"){
    ggplot(monitor_data) +
      aes(x=end_time, y=flow_time) +
      geom_line(alpha=.4, aes(group=replication)) +
      stat_smooth() +
      xlab("simulation time") +
      ylab("flow time") +
      ggtitle("Flow time evolution") +
      expand_limits(y=0)
  } else if(type=="waiting_time"){
    ggplot(monitor_data) +
      aes(x=end_time, y=waiting_time) +
      geom_line(alpha=.4, aes(group=replication)) +
      stat_smooth() +
      xlab("simulation time") +
      ylab("waiting time") +
      ggtitle("Waiting time evolution") +
      expand_limits(y=0)
  } else if(type=="activity_time"){
    ggplot(monitor_data) +
      aes(x=end_time, y=activity_time) +
      geom_line(alpha=.4, aes(group=replication)) +
      stat_smooth() +
      xlab("simulation time") +
      ylab("activity time") +
      ggtitle("Activity time evolution") +
      expand_limits(y=0)
  }
  
}