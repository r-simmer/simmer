require(R6)

#' Trajectory
#'
#' Trajectory object
#'
#' @format An \code{\link{R6Class}} generator object
#' @field name trajectory name
#' @field ... events
#' @export
Trajectory <- R6Class("Trajectory",
  public = list(
    name = NA,
    
    initialize = function(name, ...) { 
      self$name <- name; invisible(self)
      for (ev in list(...))
        private$add_event(ev)
      invisible(self)
    },
    
    show = function() {
      cat(paste0("Trajectory: ", self$name, ", ",
                 private$n_events, " events\n"))
      ptr <- private$head
      while (!is.null(ptr)) {
        ptr$show()
        cat("\n")
        ptr <- ptr$next_event
      }
      invisible(self)
    }
  ),
  
  private = list(
    n_events = 0,
    head = NULL,
    tail = NULL,
    
    add_event = function(ev) {
      if (!inherits(ev, "Event"))
        stop("not an event")
      if (is.null(private$head))
        private$head <- ev
      else
        private$tail$next_event <- ev
      private$tail <- ev
      private$n_events <- private$n_events + 1
    }
  )
)

#' Branch
#'
#' Break a trajectory in several branches
#'
#' @format An \code{\link{R6Class}} generator object
#' @export
Branch <- R6Class("Branch", inherit = Event,
  public = list(
    name = "Branch",
    
    initialize = function() {
    }
  ),
  
  active = list(
    next_event = function() {
      1
    }
  )
)
