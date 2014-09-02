#' @export
TimeValueMonitor <- setRefClass("TimeValueMonitor",
                                fields = list(
                                  data = "data.frame")
)


TimeValueMonitor$methods(initialize = function(...){
  callSuper(...)
  .self$data <- data.frame(t=0, v=0)
  .self
})

TimeValueMonitor$methods(record = function(t, v){
  data<<-rbind(data, data.frame(t, v))
})

TimeValueMonitor$methods(get_last_value = function(){
  return(data[nrow(data),"v"])
})
