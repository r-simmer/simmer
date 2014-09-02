# 
# #' @export
# Resource<-setRefClass("Resource", 
#                       fields = list(
#                         name = "character",
#                         capacity = "numeric",
#                         monitor = "TimeValueMonitor"
#                         )
# )
# 
# 
# ResourceRequirement <- setRefClass("ResourceRequirement", field = list(name = "character",
#                                                                        amount = "numeric",
#                                                                        fulfilled = "logical",
#                                                                        resource_obj = "Resource"))
# ResourceRequirement$methods(initialize = function(...){
#   callSuper(...)
#   .self$amount <- 1
#   .self$fulfilled <- FALSE
# })
