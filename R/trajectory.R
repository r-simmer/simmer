require(R6)

#' Trajectory
#'
#' Trajectory object
#'
#' @field name trajectory name
#' @format An \code{\link{R6Class}} generator object
#' @examples
#' t0 <- Trajectory$new("my trajectory") $
#'   ## add an intake activity
#'   seize("nurse", 1) $
#'   timeout(function() rnorm(1, 15)) $
#'   release("nurse", 1) $
#'   ## add a consultation activity
#'   seize("doctor", 1) $
#'   timeout(function() rnorm(1, 20)) $
#'   release("doctor", 1) $
#'   ## add a planning activity
#'   seize("administration", 1) $
#'   timeout(function() rnorm(1, 5)) $
#'   release("administration", 1)
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
                 private$n_activities, " activities\n"))
      ptr <- self$get_head()
      while (!is.null(ptr)) {
        ptr$show()
        cat("\n")
        ptr <- ptr$next_activity
      }
      invisible(self)
    },
    
    get_head = function() { private$head },
    
    get_tail = function() { private$tail },
    
    get_n_activities = function() { private$n_activities },
    
    seize = function(resource, amount) {
      private$add_activity(SeizeActivity$new(resource, amount))
    },
    
    release = function(resource, amount) {
      private$add_activity(ReleaseActivity$new(resource, amount))
    },
    
    timeout = function(duration) {
      private$add_activity(TimeoutActivity$new(duration))
    },
    
    branch = function(prob, merge=T, trj) {
      if (!inherits(trj, "Trajectory"))
        stop("not a trajectory")
      private$tail$next_activity <- c(trj$get_head(), prob)
      if (merge)
        private$merge <- c(private$merge, trj$get_tail())
      private$n_activities <- private$n_activities + trj$get_n_activities()
      invisible(self)
    }
  ),
  
  private = list(
    n_activities = 0,
    head = NULL,
    tail = NULL,
    merge = NULL,
    
    add_activity = function(ev) {
      if (!inherits(ev, "Activity"))
        stop("not an activity")
      if (is.null(private$head))
        private$head <- ev
      else if (length(private$merge)) {
        for (i in private$merge) i$next_activity <- c(ev, 1)
        private$merge = NULL
      } else
        private$tail$next_activity <- c(ev, 1)
      private$tail <- ev
      private$n_activities <- private$n_activities + 1
      invisible(self)
    }
  )
)
