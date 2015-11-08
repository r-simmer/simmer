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
#'   \item{duration}{the delay (negative values are automatically converted to positive ones)}
#' }
#' \preformatted{## Add n branches in the trajectory
#' Trajectory$branch(option, merge, ...)
#' }\describe{
#'   \item{option}{a function that must return a number between 1:n to select a branch}
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
#'   branch(function() sample(1:2, 1), merge=c(TRUE, FALSE), 
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
#' @importFrom R6 R6Class
#' @importFrom Rcpp evalCpp
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
        activity_show_(ptr, indent)
        ptr <- activity_get_next_(ptr)
      }
      activity_show_(ptr, indent)
    },
    
    get_head = function() { private$head },
    
    get_tail = function() { private$tail },
    
    get_n_activities = function() { private$n_activities },
    
    seize = function(resource, amount) {
      resource <- evaluate_value(resource)
      amount <- evaluate_value(amount)
      private$add_activity(Seize__new(resource, amount))
    },
    
    release = function(resource, amount) {
      resource <- evaluate_value(resource)
      amount <- evaluate_value(amount)
      private$add_activity(Release__new(resource, amount))
    },
    
    timeout = function(duration) {
      if (!is.function(duration)) 
        stop(paste0(self$name, ": duration must be callable"))
      private$add_activity(Timeout__new(duration))
    },
    
    branch = function(option, merge, ...) {
      trj <- list(...)
      if (!is.function(option)) 
        stop(paste0(self$name, ": option must be callable"))
      if (length(merge) != length(trj))
        stop("the number of elements does not match")
      for (i in trj) if (!inherits(i, "Trajectory"))
        stop("not a trajectory")
      private$add_activity(Branch__new(option, merge, trj))
    }
  ),
  
  private = list(
    n_activities = 0,
    head = NULL,
    tail = NULL,
    
    add_activity = function(activity) {
      if (is.null(private$head))
        private$head <- activity
      else
        activity_set_next_(private$tail, activity)
      private$tail <- activity
      private$n_activities <- private$n_activities + activity_get_n_(activity)
      invisible(self)
    }
  )
)
