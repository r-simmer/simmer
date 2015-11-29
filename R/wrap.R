#' @importFrom R6 R6Class
Simmer.wrap <- R6Class("Simmer.wrap",
  public = list(
    initialize = function(env) {
      if (!inherits(env, "Simmer")) stop("not a simmer object")
      
      private$arrivals <- env$get_mon_arrivals()
      private$resources <- env$get_mon_resources()
      for (res in levels(factor(private$resources$resource))) {
        private$capacity[[res]] <- env$get_capacity(res)
        private$queue_size[[res]] <- env$get_queue_size(res)
      }
      invisible(self)
    },
    
    get_mon_arrivals = function(include_attrs=FALSE) { private$arrivals },
    get_mon_resources = function() { private$resources },
    get_capacity = function(name) {
      if (!(name %in% names(private$capacity)))
        stop("resource not found")
      private$capacity[[name]]
    },
    get_queue_size = function(name) {
      if (!(name %in% names(private$queue_size)))
        stop("resource not found")
      private$queue_size[[name]]
    }
  ),
  
  private = list(
    arrivals = NA,
    resources = NA,
    capacity = list(),
    queue_size = list()
  )
)

#' Wrap a simulation environment
#'
#' This function extracts the monitored data from a simulation environment 
#' making it accessible through the same methods. Only useful if you want 
#' to parallelize heavy replicas (see the example below), because the C++ 
#' simulation backend is destroyed when the threads exit.
#' 
#' @param env the simulation environment.
#' @return Returns a simulation wrapper.
#' @seealso Other methods to deal with a simulation wrapper:
#' \link{get_mon_arrivals}, \link{get_mon_resources}, 
#' \link{get_capacity}, \link{get_queue_size}.
#' @examples
#' library(parallel)
#' 
#' mm1 <- create_trajectory() %>%
#'   seize("server", 1) %>%
#'   timeout(function() rexp(1, 2)) %>%
#'   release("server", 1)
#' 
#' envs <- mclapply(1:4, function(i) {
#'   simmer("M/M/1 example") %>%
#'     add_resource("server", 1) %>%
#'     add_generator("customer", mm1, function() rexp(1, 1)) %>%
#'     run(100) %>%
#'     wrap()
#' })
#' 
#' plot_resource_usage(envs, "server")
#' @export
wrap <- function(env) Simmer.wrap$new(env)
