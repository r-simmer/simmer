
Event <- setRefClass("Event", fields = list(description = "character",
                                            event_id = "character",
                                            entity_name = "character",
                                            entity_index = "numeric",
                                            early_start = "numeric",
                                            start_time = "numeric",
                                            end_time = "numeric",
                                            required_resources ="list",
                                            amount = "numeric",
                                            successor = "character",
                                            duration = "numeric"))




## Event init method (inits monitor data.frame)
Event$methods(initialize = function(...){
  callSuper(...)
  .self$end_time <- Inf
  .self
  
})

Event$methods(has_started = function(){
  if(length(.self$start_time)>0){
    return(TRUE)
  } else {
    return(FALSE)
  }
})