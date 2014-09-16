#' @export
TimeValueMonitor <- setRefClass("TimeValueMonitor",
                                fields = list(
                                  data = "matrix")
)


TimeValueMonitor$methods(initialize = function(...){
  callSuper(...)
  .self$data <- matrix(ncol=2, nrow=0)
  .self$data <- rbind(.self$data, c(0,0))
  colnames(.self$data) <- c("t","v")
  .self
})

TimeValueMonitor$methods(record = function(t, v){
  data<<-rbind(data, c(t, v))
})

TimeValueMonitor$methods(get_last_value = function(){
  return(data[nrow(data),"v"])
})
