.summarise_monitor_values<-function(monitor_data, type){
  monitor_data %>%
    group_by(time, replication, resource) %>%
    summarise(value = sum(value)) %>%
    group_by(replication, resource) %>%
    mutate(mean = c(0, cumsum(head(value,-1) * diff(time)) / tail(time,-1)),
           type = type
    ) %>% ungroup()

}

#' Get the server monitor values of a specified resource
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
  ) %>% .summarise_monitor_values("server")
  
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
  ) %>% .summarise_monitor_values("queue")
  
}

#' Get the system-wide monitor values (queue+server) of a specified resource
#' 
#' @param sim_obj the simulation object
#' @param resource_name the name of the resource
#' @export
get_resource_system_mon_values<-function(sim_obj, resource_name){
  queue <- get_resource_queue_mon_values(sim_obj, resource_name)
  server <- get_resource_serve_mon_values(sim_obj, resource_name)
  step_queue <- stepfun(tail(queue$time, -1), queue$value)
  step_server <- stepfun(tail(server$time, -1), server$value)
  monitor_data <- rbind(queue, server) %>% .summarise_monitor_values("system")
  monitor_data$value <- step_queue(monitor_data$time) + step_server(monitor_data$time)
  monitor_data
  
}