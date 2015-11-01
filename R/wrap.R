#' Simmer.wrap
#'
#' Extracts the simulation data from a Simmer object making it accessible 
#' through the same methods. Only useful if you want to parallelize heavy
#' replicas (see the example below), because the C++ simulation backend is
#' destroyed when the threads exit.
#'
#' @seealso \link{Simmer}
#' @section Methods:
#' \preformatted{## Object creation
#' Simmer.wrap$new(simmer)
#' }\describe{
#'   \item{simmer}{the \link{Simmer} object}
#' }
#' \preformatted{## Get the arrival statistics
#' Simmer.wrap$get_mon_arrivals()
#' }
#' \preformatted{## Get the resource statistics
#' Simmer.wrap$get_mon_resources()
#' }
#' \preformatted{## Get the capacity of a resource
#' Simmer.wrap$get_res_capacity(name)
#' }\describe{
#'   \item{name}{the name of the resource}
#' }
#' \preformatted{## Get the queue size of a resource
#' Simmer.wrap$get_res_queue_size(name)
#' }\describe{
#'   \item{name}{the name of the resource}
#' }
#' @format NULL
#' @usage NULL
#' @examples 
#' library(parallel)
#' 
#' mm1 <- Trajectory$new() $
#'   seize("server", 1) $
#'   timeout(function() rexp(1, 2)) $
#'   release("server", 1)
#' 
#' reps <- mclapply(1:4, function(i) {
#'   Simmer.wrap$new(
#'   Simmer$new("M/M/1 example") $
#'     add_resource("server", 1) $
#'     add_generator("customer", mm1, function() rexp(1, 1)) $
#'     run(100)
#'   )
#' })
#' 
#' plot_resource_usage(reps, "server")
#' @import R6
#' @export
Simmer.wrap <- R6Class("Simmer.wrap",
  public = list(
    initialize = function(simmer) {
      if (!inherits(simmer, "Simmer")) stop("not a simmer object")
      
      private$arrivals <- simmer$get_mon_arrivals()
      private$resources <- simmer$get_mon_resources()
      for (res in levels(factor(private$resources$resource))) {
        private$capacity[[res]] <- simmer$get_res_capacity(res)
        private$queue_size[[res]] <- simmer$get_res_queue_size(res)
      }
      invisible(self)
    },
    
    get_mon_arrivals = function() { private$arrivals },
    get_mon_resources = function() { private$resources },
    get_res_capacity = function(name) { private$capacity[[name]] },
    get_res_queue_size = function(name) { private$queue_size[[name]] }
  ),
  
  private = list(
    arrivals = NA,
    resources = NA,
    capacity = list(),
    queue_size = list()
  )
)
