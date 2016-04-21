#' @importFrom R6 R6Class
simmer.wrap <- R6Class("simmer.wrap",
  public = list(
    name = NA,
    
    initialize = function(env) {
      if (!inherits(env, "simmer")) stop("not a simmer object")
      
      self$name <- env$name
      private$now_val <- env$now()
      private$peek_val <- env$peek()
      private$res <- env$get_resources()
      private$gen <- env$get_generators()
      private$arrivals <- env$get_mon_arrivals()
      private$arrivals_res <- env$get_mon_arrivals(TRUE)
      private$attributes <- env$get_mon_attributes()
      private$resources_all <- env$get_mon_resources(data=c("counts", "limits"))
      private$resources_counts <- env$get_mon_resources(data="counts")
      private$resources_limits <- env$get_mon_resources(data="limits")
      for (name in names(private$gen)) {
        private$n_generated[[name]] <- env$get_n_generated(name)
      }
      for (name in names(private$res)) {
        private$capacity[[name]] <- env$get_capacity(name)
        private$queue_size[[name]] <- env$get_queue_size(name)
        private$server_count[[name]] <- env$get_server_count(name)
        private$queue_count[[name]] <- env$get_queue_count(name)
      }
      self
    },
    
    print = function() {
      cat(paste0(
        "simmer wrapper: ", self$name,
        " | now: ", self$now(), " | next: ", self$peek(), "\n"
      ))
      for (name in names(private$res))
        cat(paste0(
          "{ Resource: ", name, 
          " | monitored: ", private$res[[name]],
          " | server status: ", self$get_server_count(name), 
          "(", self$get_capacity(name), ")",
          " | queue status: ", self$get_queue_count(name),
          "(", self$get_queue_size(name), ") }\n"
        ))
      for (name in names(private$gen))
        cat(paste0(
          "{ Generator: ", name,
          " | monitored: ", private$gen[[name]],
          " | n_generated: ", self$get_n_generated(name), " }\n"
        ))
    },
    
    now = function() private$now_val,
    peek = function() private$peek_val,
    
    get_mon_arrivals = function(per_resource=FALSE) {
      if (per_resource) private$arrivals_res
      else private$arrivals 
    },
    get_mon_attributes = function() { private$attributes },
    get_mon_resources = function(data="counts") {
      if (all(!data %in% c("counts", "limits")))
        stop("parameter 'data' should be 'counts', 'limits' or both")
      if (all(c("counts", "limits") %in% data))
        private$resources_all
      else if (all(data %in% "counts"))
        private$resources_counts
      else private$resources_limits
    },
    get_n_generated = function(name) {
      if (!(name %in% names(private$gen)))
        stop("generator not found")
      private$n_generated[[name]]
    },
    get_capacity = function(name) {
      if (!(name %in% names(private$res)))
        stop("resource not found")
      private$capacity[[name]]
    },
    get_queue_size = function(name) {
      if (!(name %in% names(private$res)))
        stop("resource not found")
      private$queue_size[[name]]
    },
    get_server_count = function(name) {
      if (!(name %in% names(private$res)))
        stop("resource not found")
      private$server_count[[name]]
    },
    get_queue_count = function(name) {
      if (!(name %in% names(private$res)))
        stop("resource not found")
      private$queue_count[[name]]
    }
  ),
  
  private = list(
    now_val = NA,
    peek_val = NA,
    res = NA,
    gen = NA,
    arrivals = NA,
    arrivals_res = NA,
    attributes = NA,
    resources_all = NA,
    resources_counts = NA,
    resources_limits = NA,
    n_generated = list(),
    capacity = list(),
    queue_size = list(),
    server_count = list(),
    queue_count = list()
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
#' 
#' @return Returns a simulation wrapper.
#' @seealso Other methods to deal with a simulation wrapper:
#' \link{get_mon_arrivals}, \link{get_mon_attributes}, \link{get_mon_resources}, 
#' \link{get_n_generated}, \link{get_capacity}, \link{get_queue_size},
#' \link{get_server_count}, \link{get_queue_count}.
#' @export
#' 
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
wrap <- function(env) simmer.wrap$new(env)
