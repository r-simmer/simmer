require(R6)

Event <- R6Class("Event",
  public = list(
    name = NA,

    show = function() {
      cat(paste0("{ Event: ", self$name, " | "))
      for (i in names(private)) {
        if (is.function(private[[i]]))
          cat(i, ": function(), ", sep = "")
        else if (is.environment(private[[i]]))
          cat(i, ": env, ", sep = "")
        else
          cat(i, ": ", private[[i]], ", ", sep = "")
      }
      cat("}")
    },
    
    run = function() { stop("not implemented") }
  ),
  
  active = list(
    next_event = function(ev) {
      if (missing(ev)) return(private$ptr)
      else private$ptr <- ev
    }
  ),
  
  private = list(
    ptr = NULL
  )
)

#' SeizeEvent
#'
#' Seize a resource by an amount
#'
#' @format An \code{\link{R6Class}} generator object
#' @field resource the resource name
#' @field amount the amount to seize
#' @export
SeizeEvent <- R6Class("SeizeEvent", inherit = Event,
  public = list(
    name = "Seize",
    
    initialize = function(resource, amount) {
      private$resource <- evaluate_value(resource)
      private$amount <- evaluate_value(amount)
    },
    
    run = function() {}
  ),
  
  private = list(
    resource = NA,
    amount = NA
  )
)

#' ReleaseEvent
#'
#' Release a resource by an amount
#'
#' @format An \code{\link{R6Class}} generator object
#' @field resource the resource name
#' @field amount the amount to release
#' @export
ReleaseEvent <- R6Class("ReleaseEvent", inherit = Event,
  public = list(
    name = "Release",
    
    initialize = function(resource, amount) {
      private$resource <- evaluate_value(resource)
      private$amount <- evaluate_value(amount)
    },
    
    run = function() {}
  ),
  
  private = list(
    resource = NA,
    amount = NA
  )
)

#' TimeoutEvent
#'
#' Wait for a given delay
#'
#' @format An \code{\link{R6Class}} generator object
#' @field duration a function that generates a delay
#' @export
TimeoutEvent <- R6Class("TimeoutEvent", inherit = Event,
  public = list(
    name = "Timeout",
    
    initialize = function(duration) {
      if (!is.function(duration)) 
        stop(paste0(self$name, ": duration must be callable"))
      private$duration <- evaluate_value(duration)
    },
    
    run = function() {}
  ),
  
  private = list(
    duration = NA
  )
)
