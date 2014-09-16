setClass("Event", representation(description = "character",
                                 event_id = "character",
                                 successor_id = "character"))


setClass("SeizeEvent", representation(resource_name = "character",
                                             resource_amount = "numeric"),
         contains = "Event")


setClass("ReleaseEvent", representation(resource_name = "character",
                                      resource_amount = "numeric"),
         contains = "Event")


setClass("TimeoutEvent", representation(description = "character", 
                                        duration = "character"),
         contains = "Event")
