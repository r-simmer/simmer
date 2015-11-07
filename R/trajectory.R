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
#'   branch(prob=c(0.5, 0.5), merge=c(TRUE, FALSE), 
#'     Trajectory$new("branch1") $
#'       timeout(function() 1),
#'     Trajectory$new("branch2") $
#'       timeout(function() rexp(1, 3)) $
#'       release("server", 1)
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
    
    show = function(indent=0) {
      margin <- paste(rep(" ", indent), collapse="")
      cat(paste0(margin, "Trajectory: ", self$name, ", ",
                 private$n_activities, " activities\n"))
      ptr <- private$head
      while (!identical(ptr, private$tail)) {
        ptr$show(indent)
        ptr <- ptr$next_activity
      }
      ptr$show(indent)
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
    
    branch = function(prob, merge=TRUE, ...) {
      private$add_activity(BranchActivity$new(prob, merge, ...))
    }
  ),
  
  private = list(
    n_activities = 0,
    head = NULL,
    tail = NULL,
    
    add_activity = function(activity) {
      if (!inherits(activity, "Activity"))
        stop("not an activity")
      if (is.null(private$head))
        private$head <- activity
      else
        private$tail$next_activity <- activity
      private$tail <- activity
      private$n_activities <- private$n_activities + activity$n
      invisible(self)
    }
  )
)
