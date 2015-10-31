#' plot usage of a resource over time
#' 
#' plot the usage of a resource over the simulation time frame
#' @param simmer the simulation environment
#' @param resource_name the name of the resource (character value)
#' @param type the parts of the resource to be plotted
#' @param steps adds the changes in the resource usage
#' @export
plot_resource_usage <- function(simmer, resource_name, type=c("queue", "server", "system"), steps = FALSE) {
  require(ggplot2)
  require(dplyr)
  require(tidyr)
  
  monitor_data <- simmer$get_mon_resources()
  if (!nrow(monitor_data)) stop("no data available")
  
  monitor_data <- monitor_data %>% 
    filter(resource == resource_name) %>%
    gather(type, value, 2:4) %>%
    mutate(type = factor(type)) %>%
    filter(type %in% type) %>%
    group_by(resource, type) %>%
    mutate(mean = cumsum(value * diff(c(0,time))) / time) %>% 
    ungroup()
  
  queue_size <- simmer$get_res_queue_size(resource_name)
  capacity <- simmer$get_res_capacity(resource_name)
  system <- capacity + queue_size
  
  plot_obj<-
    ggplot(monitor_data) +
    aes(x=time, color=type) +
    geom_line(aes(y=mean, group=type)) +
    ggtitle(paste("Resource usage:", resource_name)) +
    scale_y_continuous(breaks=seq(0,1000,1)) +
    scale_color_discrete(limits=levels(monitor_data$type)) +
    ylab("in use") +
    xlab("time") +
    expand_limits(y=0)
  
  if("server" %in% type){
    plot_obj<- plot_obj +
      geom_hline(y=capacity, lty=2, color="red")
  }
  if("queue" %in% type && queue_size >= 0){
    plot_obj<- plot_obj +
      geom_hline(y=queue_size, lty=2, color="green")
  }
  if("system" %in% type && queue_size >= 0){
    plot_obj<- plot_obj +
      geom_hline(y=system, lty=2, color="blue")
  }
   
  if(steps == T){
    plot_obj<- plot_obj +
      geom_step(aes(y=value), alpha=.4)
  }
  
  plot_obj
}

#' plot utilization of resources
#' 
#' plot the utilization of specified resources in the simulation
#' @param simmer the simulation environment
#' @param resources a character vector with at least one resource specified - e.g. "c('res1','res2')"
#' @export
plot_resource_utilization <- function(simmer, resources){
  require(ggplot2)
  require(dplyr)
  require(tidyr)
  require(scales)
  
  monitor_data <- simmer$get_mon_resources()
  if (!nrow(monitor_data)) stop("no data available")
  
  monitor_data <- monitor_data %>% 
    filter(resource %in% resources) %>%
    gather(type, value, 2:4) %>%
    mutate(type = factor(type)) %>%
    filter(type == "server") %>%
    group_by(resource) %>%
    mutate(capacity = simmer$get_res_capacity(resource[[1]])) %>% 
    # group_by(replication) %>%
    mutate(runtime = max(time)) %>%
    group_by(resource, capacity, runtime) %>%
    mutate(in_use = (time-lag(time)) * lag(value)) %>%
    group_by(resource, capacity, runtime) %>%
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

#' plot evolution of arrival times
#' 
#' plot the evolution of arrival related times (flow, activity and waiting time)
#' @param simmer the simulation environment
#' @param type one of c("flow_time","activity_time","waiting_time")
#' @export
plot_evolution_arrival_times <- function(simmer, type=c("flow_time","activity_time","waiting_time")){
  require(ggplot2)
  require(dplyr)
  require(tidyr)
  
  monitor_data <- simmer$get_mon_arrivals()
  if (!nrow(monitor_data)) stop("no data available")
  
  monitor_data <- monitor_data %>%
    mutate(flow_time = end_time - start_time,
           waiting_time = flow_time - activity_time)

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