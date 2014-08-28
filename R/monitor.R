

Monitor <- setRefClass("Monitor",
                       fields = list(
                         data = "data.frame")
)


Monitor$methods(initialize = function(...){
  callSuper(...)
  .self$data <- data.frame(t=0, v=0)
  .self
})

Monitor$methods(record = function(t, v){
  data<<-rbind(data, data.frame(t, v))
})

Monitor$methods(get_last_value = function(t, v){
  return(data[nrow(data),"v"])
})
