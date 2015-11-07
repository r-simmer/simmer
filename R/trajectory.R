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
#' Trajectory$branch(prob, merge, ...)
#' }\describe{
#'   \item{prob}{a vector of n probabilities, one for each branch}
#'   \item{merge}{a vector of n booleans that indicate whether the arrival must continue executing activities after each branch or not}
#'   \item{...}{n Trajectory objects describing each branch}
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
      resource <- evaluate_value(resource)
      amount <- evaluate_value(amount)
      private$add_activity(Seize$new(resource, amount))
    },
    
    release = function(resource, amount) {
      resource <- evaluate_value(resource)
      amount <- evaluate_value(amount)
      private$add_activity(Release$new(resource, amount))
    },
    
    timeout = function(duration) {
      if (!is.function(duration)) 
        stop(paste0(self$name, ": duration must be callable"))
      private$add_activity(Timeout$new(duration))
    },
    
    branch = function(prob, merge, ...) {
      trj <- list(...)
      if (sum(prob) != 1)
        stop("prob must sum 1")
      if ((length(prob) != length(merge)) || (length(prob) != length(trj)))
        stop("the number of elements does not match")
      for (i in trj) if (!inherits(i, "Trajectory"))
        stop("not a trajectory")
      private$add_activity(Branch$new(prob, merge, trj))
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
