require(R6)

#' Trajectory
#'
#' Trajectory object
#'
#' @field name trajectory name
#' @format An \code{\link{R6Class}} generator object
#' @examples
#' t0 <- Trajectory$new("my trajectory") $
#'   seize("server", 1) $
#'   timeout(function() rexp(1, 1)) $
#'   release("server", 1)
#' @import R6
#' @export
Trajectory <- R6Class("Trajectory",
  public = list(
    name = NA,
    
    initialize = function(name="anonymous") { 
      self$name <- evaluate_value(name)
      invisible(self)
    },
    
    show = function() {
      cat(paste0("Trajectory: ", self$name, ", ",
                 private$n_events, " events\n"))
      ptr <- self$get_head()
      while (!is.null(ptr)) {
        ptr$show()
        cat("\n")
        ptr <- ptr$next_event
      }
      invisible(self)
    },
    
    get_head = function() { private$head },
    
    get_tail = function() { private$tail },
    
    get_n_events = function() { private$n_events },
    
    seize = function(resource, amount) {
      private$add_event(SeizeEvent$new(resource, amount))
    },
    
    release = function(resource, amount) {
      private$add_event(ReleaseEvent$new(resource, amount))
    },
    
    timeout = function(duration) {
      private$add_event(TimeoutEvent$new(duration))
    },
    
    branch = function(prob, merge=T, trj) {
      if (!inherits(trj, "Trajectory"))
        stop("not a trajectory")
      private$tail$next_event <- c(trj$get_head(), prob)
      if (merge)
        private$merge <- c(private$merge, trj$get_tail())
      private$n_events <- private$n_events + trj$get_n_events()
      invisible(self)
    }
  ),
  
  private = list(
    n_events = 0,
    head = NULL,
    tail = NULL,
    merge = NULL,
    
    add_event = function(ev) {
      if (!inherits(ev, "Event"))
        stop("not an event")
      if (is.null(private$head))
        private$head <- ev
      else if (length(private$merge)) {
        for (i in private$merge) i$next_event <- c(ev, 1)
        private$merge = NULL
      } else
        private$tail$next_event <- c(ev, 1)
      private$tail <- ev
      private$n_events <- private$n_events + 1
      invisible(self)
    }
  )
)
