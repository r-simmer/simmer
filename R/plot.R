#' plot usage of a resource over time
#' 
#' plot the usage of a resource over the simulation time frame
#' @param reps a single simmer environment or a list of environments representing several replications
#' @param resource_name the name of the resource (character value)
#' @param items the components of the resource to be plotted
#' @param steps adds the changes in the resource usage
#' @importFrom magrittr %>%
#' @export
plot_resource_usage <- function(reps, resource_name, items=c("queue", "server", "system"), steps = FALSE) {
  if (!is.list(reps)) reps <- list(reps)
  
  monitor_data <- do.call(rbind, lapply(1:length(reps), function(i) {
    stats <- reps[[i]]$get_mon_resources()
    stats$replication <- i
    stats
  }))
  
  monitor_data <- monitor_data %>% 
    dplyr::filter(resource == resource_name) %>%
    tidyr::gather(item, value, 2:4) %>%
    dplyr::mutate(item = factor(item)) %>%
    dplyr::filter(item %in% items) %>%
    dplyr::group_by(resource, replication, item) %>%
    dplyr::mutate(mean = cumsum(value * diff(c(0,time))) / time) %>% 
    dplyr::ungroup()
  
  queue_size <- reps[[1]]$get_queue_size(resource_name)
  capacity <- reps[[1]]$get_capacity(resource_name)
  system <- capacity + queue_size
  
  plot_obj<-
    ggplot2::ggplot(monitor_data) +
    ggplot2::aes(x=time, color=item) +
    ggplot2::geom_line(ggplot2::aes(y=mean, group=interaction(replication, item))) +
    ggplot2::ggtitle(paste("Resource usage:", resource_name)) +
    ggplot2::scale_y_continuous(breaks=seq(0,1000,1)) +
    ggplot2::scale_color_discrete(limits=levels(monitor_data$item)) +
    ggplot2::ylab("in use") +
    ggplot2::xlab("time") +
    ggplot2::expand_limits(y=0)
  
  if("server" %in% items){
    plot_obj <- plot_obj +
      ggplot2::geom_hline(y=capacity, lty=2, color="red")
  }
  if("queue" %in% items && queue_size >= 0){
    plot_obj <- plot_obj +
      ggplot2::geom_hline(y=queue_size, lty=2, color="green")
  }
  if("system" %in% items && queue_size >= 0){
    plot_obj <- plot_obj +
      ggplot2::geom_hline(y=system, lty=2, color="blue")
  }
   
  if(steps == T){
    plot_obj <- plot_obj +
      ggplot2::geom_step(ggplot2::aes(y=value, group=replication), alpha=.4)
  }
  
  plot_obj
}

#' plot utilization of resources
#' 
#' plot the utilization of specified resources in the simulation
#' @param reps a single simmer environment or a list of environments representing several replications
#' @param resources a character vector with at least one resource specified - e.g. "c('res1','res2')"
#' @importFrom magrittr %>%
#' @export
plot_resource_utilization <- function(reps, resources){
  if (!is.list(reps)) reps <- list(reps)
  
  monitor_data <- do.call(rbind, lapply(1:length(reps), function(i) {
    stats <- reps[[i]]$get_mon_resources()
    stats$replication <- i
    stats
  }))
  
  monitor_data <- monitor_data %>% 
    dplyr::filter(resource %in% resources) %>%
    tidyr::gather(item, value, 2:4) %>%
    dplyr::mutate(item = factor(item)) %>%
    dplyr::filter(item == "server") %>%
    dplyr::group_by(resource) %>%
    dplyr::mutate(capacity = reps[[1]]$get_capacity(resource[[1]])) %>% 
    dplyr::group_by(replication) %>%
    dplyr::mutate(runtime = max(time)) %>%
    dplyr::group_by(resource, replication, capacity, runtime) %>%
    dplyr::mutate(in_use = (time-lag(time)) * lag(value)) %>%
    dplyr::group_by(resource, replication, capacity, runtime) %>%
    dplyr::summarise(in_use = sum(in_use, na.rm=T)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(utilization = in_use / capacity / runtime) %>%
    dplyr::group_by(resource, capacity) %>%
    dplyr::summarise(Q25 = quantile(utilization, .25),
              Q50 = quantile(utilization, .5),
              Q75 = quantile(utilization, .75))
  
  ggplot2::ggplot(monitor_data) +
    ggplot2::aes(x=resource, y=Q50, ymin=Q25, ymax=Q75) + 
    ggplot2::geom_bar(stat="identity") + 
    ggplot2::geom_errorbar(width=.25, color="black") +
    ggplot2::ggtitle("Resource utilization") +
    ggplot2::scale_y_continuous(labels=scales::percent, limits=c(0,1), breaks=seq(0,2,.2)) +
    ggplot2::ylab("utilization")
}

#' plot evolution of arrival times
#' 
#' plot the evolution of arrival related times (flow, activity and waiting time)
#' @param reps a single simmer environment or a list of environments representing several replications
#' @param type one of c("flow_time","activity_time","waiting_time")
#' @importFrom magrittr %>%
#' @export
plot_evolution_arrival_times <- function(reps, type=c("flow_time","activity_time","waiting_time")){
  if (!is.list(reps)) reps <- list(reps)
  
  monitor_data <- do.call(rbind, lapply(1:length(reps), function(i) {
    stats <- reps[[i]]$get_mon_arrivals()
    stats$replication <- i
    stats
  }))
  
  monitor_data <- monitor_data %>%
    dplyr::mutate(flow_time = end_time - start_time,
                  waiting_time = flow_time - activity_time)

  if(type=="flow_time"){
    ggplot2::ggplot(monitor_data) +
      ggplot2::aes(x=end_time, y=flow_time) +
      ggplot2::geom_line(alpha=.4, ggplot2::aes(group=replication)) +
      ggplot2::stat_smooth() +
      ggplot2::xlab("simulation time") +
      ggplot2::ylab("flow time") +
      ggplot2::ggtitle("Flow time evolution") +
      ggplot2::expand_limits(y=0)
  } else if(type=="waiting_time"){
    ggplot2::ggplot(monitor_data) +
      ggplot2::aes(x=end_time, y=waiting_time) +
      ggplot2::geom_line(alpha=.4, ggplot2::aes(group=replication)) +
      ggplot2::stat_smooth() +
      ggplot2::xlab("simulation time") +
      ggplot2::ylab("waiting time") +
      ggplot2::ggtitle("Waiting time evolution") +
      ggplot2::expand_limits(y=0)
  } else if(type=="activity_time"){
    ggplot2::ggplot(monitor_data) +
      ggplot2::aes(x=end_time, y=activity_time) +
      ggplot2::geom_line(alpha=.4, ggplot2::aes(group=replication)) +
      ggplot2::stat_smooth() +
      ggplot2::xlab("simulation time") +
      ggplot2::ylab("activity time") +
      ggplot2::ggtitle("Activity time evolution") +
      ggplot2::expand_limits(y=0)
  }
}