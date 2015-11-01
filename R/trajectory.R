require(R6)

#' Trajectory
#'
#' A trajectory is a chain of activities followed by arrivals of the same type.
#'
#' @seealso \link{Simmer}
#' @section Methods:
#' \preformatted{## Object creation
#' Trajectory$new(name="anonymous")
#' }\describe{
#'   \item{name}{the name of the trajectory}
#' }
#' \preformatted{## Show the trajectory
#' Trajectory$show() 
#' }\preformatted{## Get the first activity
#' Trajectory$get_head()
#' }
#' \preformatted{## Get the last activity
#' Trajectory$get_tail()
#' }
#' \preformatted{## Get the total number of activities in the trajectory
#' Trajectory$get_n_activities()
#' }
#' \preformatted{## Add a seize activity
#' Trajectory$seize(resource, amount)
#' }\describe{
#'   \item{resource}{the name of the resource}
#'   \item{amount}{the amount needed}
#' }
#' \preformatted{## Add a release activity
#' Trajectory$release(resource, amount)
#' }\describe{
#'   \item{resource}{the name of the resource}
#'   \item{amount}{the amount needed}
#' }
#' \preformatted{## Add a delay activity
#' Trajectory$timeout(duration)
#' }\describe{
#'   \item{duration}{the delay}
#' }
#' \preformatted{## Add a branch in the trajectory
#' Trajectory$branch(prob, merge=TRUE, trj)
#' }\describe{
#'   \item{prob}{the probability for an arrival of traversing this branch}
#'   \item{merge}{whether the arrival must continue executing activities after this branch or not}
#'   \item{trj}{a Trajectory object describing this branch}
#' }
#' @format NULL
#' @usage NULL
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
#' 
#' t0$show()
#' 
#' t1 <- Trajectory$new("trajectory with a branch") $
#'   seize("server", 1) $
#'   ## 50-50 chance for each branch
#'   branch(prob=0.5, merge=TRUE, Trajectory$new("branch1") $
#'     timeout(function() 1)
#'   ) $
#'   branch(prob=0.5, merge=FALSE, Trajectory$new("branch2") $
#'     timeout(function() rexp(1, 3)) $
#'     release("server", 1)
#'   ) $
#'   ## only the first branch continues here
#'   release("server", 1) $
#'   timeout(function() 2)
#' 
#' t1$show()
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
    
    branch = function(prob, merge=TRUE, trj) {
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
