# 
# #' TimeValueMonitor class.
# #' 
# #' Holds data related to specific point in simulation time. E.g. holds values for resource usage at time x.
# #' @export
# TimeValueMonitor <- setRefClass("TimeValueMonitor",
#                        fields = list(
#                          data = "data.frame")
# )
# 
# 
# TimeValueMonitor$methods(initialize = function(...){
#   callSuper(...)
#   .self$data <- data.frame(t=0, v=0)
#   .self
# })
# 
# TimeValueMonitor$methods(record = function(t, v){
#   data<<-rbind(data, data.frame(t, v))
# })
# 
# TimeValueMonitor$methods(get_last_value = function(t, v){
#   return(data[nrow(data),"v"])
# })
# # 
# # #' KeyValueMonitor class.
# # #' 
# # #' Holds data not related to specific point in simulation time. E.g. holds values for keys suchs as "flow_time", "activity_time". NOT USED AT THE MOMENT
# # KeyValueMonitor<-setRefClass("KeyValueMonitor", 
# #                              fields = list(
# #                                data = "list"))
# # 
# # KeyValueMonitor$methods(initialize = function(...){
# #   callSuper(...)
# #   .self$data <- list()
# #   .self
# # })
# # 
# # KeyValueMonitor$methods(record = function(key, v){
# #   data[key]<<- v
# # })
# # 
# # KeyValueMonitor$methods(get_value = function(key){
# #   data[key]
# # })
# # 
# # KeyValueMonitor$methods(record_increment = function(key, value){
# #   if(key %in% names(data)) data[key] <<- data[[key]] + value
# #   else data[key] <<- value
# # })
# # 
# # 
