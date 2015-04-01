
#' Get the monitor values of the entities in a simulation object
#' 
#' @param sim_obj the simulation object
#' @param aggregated if FALSE (default) returns the raw data, otherwise returns an aggregated row per entity
#' @export
get_entity_monitor_values<-function(sim_obj, aggregated = FALSE){
  
  dataset <- 
    do.call(rbind,
            lapply(1:length(sim_obj@simulators),function(i){
              monitor_data<-
                as.data.frame(
                  get_entity_monitor_values_(sim_obj@simulators[[i]])
                )
              monitor_data$replication<-i
              
              monitor_data
            }
            )
    )
  
  if(aggregated){
    require(dplyr)
    dataset<-
      dataset %>%
      group_by(replication, entity_id) %>%
      mutate(start_time = min(time[value==-999]),
             end_time = max(time[value==-999]),
             finished = ifelse(start_time == end_time, 0, 1),
             value_adj = ifelse(value<0, 0, value)) %>%
      group_by(replication, entity_id, time, start_time, end_time, finished) %>%
      summarise(value=max(value_adj)) %>%
      group_by(replication, entity_id, start_time, end_time, finished) %>%
      mutate(activity_time = (time-lag(time)) * lag(value)) %>%
      summarise(activity_time = sum(activity_time, na.rm=T)) %>%
      mutate(flow_time = end_time - start_time,
             waiting_time = flow_time - activity_time) %>%
      filter(finished == 1) %>%
      arrange(replication, end_time)
  }
  as.data.frame(dataset)
  
}

#' Get the serve monitor values of a specified resource
#' 
#' @param sim_obj the simulation object
#' @param resource_name the name of the resource
#' @export
get_resource_serve_mon_values<-function(sim_obj, resource_name){
  do.call(rbind,
          lapply(1:length(sim_obj@simulators),function(i){
            monitor_data<-
              as.data.frame(
                get_resource_serve_mon_values_(sim_obj@simulators[[i]], resource_name)
              )
            monitor_data$replication<-i
            monitor_data
          }
          )
  )
  
}

#' Get the queue monitor values of a specified resource
#' 
#' @param sim_obj the simulation object
#' @param resource_name the name of the resource
#' @export
get_resource_queue_mon_values<-function(sim_obj, resource_name){
  do.call(rbind,
          lapply(1:length(sim_obj@simulators),function(i){
            monitor_data<-
              as.data.frame(
                get_resource_queue_mon_values_(sim_obj@simulators[[i]], resource_name)
              )
            monitor_data$replication<-i
            monitor_data
          }
          )
  )
  
}