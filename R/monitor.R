Monitor <- R6Class("monitor",
  public = list(
    initialize = function(name, ptr, get_arrivals, get_attributes, get_resources) {
      private$name <- name
      private$ptr <- ptr
      self$get_arrivals <- function(...) get_arrivals(private$ptr, ...)
      self$get_attributes <- function(...) get_attributes(private$ptr, ...)
      self$get_resources <- function(...) get_resources(private$ptr, ...)
      self
    },

    get_arrivals = NULL,
    get_attributes = NULL,
    get_resources = NULL,

    print = function() {
      cat(paste0("simmer monitor: ", private$name, "\n"))
      invisible(self)
    },

    get_ptr = function() { private$ptr }
  ),

  private = list(
    name = NA,
    ptr = NULL
  )
)

#' Create a Monitor
#'
#' @name monitor
#' @export
monitor_mem <- function()
  Monitor$new("In memory", MemMonitor__new(), get_arrivals_, get_attributes_, get_resources_)
