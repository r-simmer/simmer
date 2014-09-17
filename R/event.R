setClass("Event", representation(description = "character",
                                 event_id = "character",
                                 successor_id = "character",
                                 successor_id_eval = "character",
                                 early_start_time = "numeric",
                                 end_time = "numeric"))


setClass("SeizeEvent", representation(resource_name = "character",
                                      resource_amount = "numeric"),
         contains = "Event")


setClass("ReleaseEvent", representation(resource_name = "character",
                                        resource_amount = "numeric"),
         contains = "Event")


setClass("TimeoutEvent", representation(description = "character", 
                                        duration = "character",
                                        duration_eval = "numeric"),
         contains = "Event")



#' @export
evaluate_event<-function(event_obj, early_start){
  event_obj@early_start_time <- early_start
  event_obj@end_time <- Inf
  
  event_obj@successor_id_eval <-  as.character(eval(parse(text=event_obj@successor_id)))
  
  if("duration" %in% slotNames(event_obj)){
    event_obj@duration_eval <- eval(parse(text=event_obj@duration))
  }
  
  event_obj
}


# #' @export
# activate_event<-function(event_obj, current_time, sim_obj){
#   if(class(event_obj) == "EventTimeout"){
#     event_obj@end_time <- current_time + event_obj@duration
#     
#     return(event_obj)
#     
#   } else if(class(event_obj) == "SeizeResource"){
#           
#     
#   }
#   
#   event_obj
# }