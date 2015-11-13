#' @importFrom R6 R6Class
#' @importFrom Rcpp evalCpp
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

#' Create a trajectory
#'
#' This function initialises a trajectory, which is a chain of activities followed 
#' by arrivals of the same type.
#' 
#' @param name the name of the trajectory.
#' @return Returns an environment that represents the trajectory.
#' @seealso Other methods to deal with trajectories:
#' \link{show_trajectory}, \link{get_head}, \link{get_tail},
#' \link{get_n_activities}, \link{seize}, \link{release}, \link{timeout}, \link{branch}.
#' @examples
#' t0 <- create_trajectory("my trajectory") %>%
#'   ## add an intake activity
#'   seize("nurse", 1) %>%
#'   timeout(function() rnorm(1, 15)) %>%
#'   release("nurse", 1) %>%
#'   ## add a consultation activity
#'   seize("doctor", 1) %>%
#'   timeout(function() rnorm(1, 20)) %>%
#'   release("doctor", 1) %>%
#'   ## add a planning activity
#'   seize("administration", 1) %>%
#'   timeout(function() rnorm(1, 5)) %>%
#'   release("administration", 1)
#' 
#' t0 %>% show_trajectory()
#' 
#' t1 <- create_trajectory("trajectory with a branch") %>%
#'   seize("server", 1) %>%
#'   ## 50-50 chance for each branch
#'   branch(function() sample(1:2, 1), merge=c(TRUE, FALSE), 
#'     create_trajectory("branch1") %>%
#'       timeout(function() 1),
#'     create_trajectory("branch2") %>%
#'       timeout(function() rexp(1, 3)) %>%
#'       release("server", 1)
#'   ) %>%
#'   ## only the first branch continues here
#'   release("server", 1) %>%
#'   timeout(function() 2)
#' 
#' t1 %>% show_trajectory()
#' @export
create_trajectory <- function(name="anonymous") Trajectory$new(name)

#' Show a trajectory
#'
#' It can be used to visualise a trajectory's internal structure.
#' 
#' @param traj the trajectory object.
#' @seealso Other methods to deal with trajectories:
#' \link{create_trajectory}, \link{get_head}, \link{get_tail},
#' \link{get_n_activities}, \link{seize}, \link{release}, \link{timeout}, \link{branch}.
#' @export
show_trajectory <- function(traj) traj$show()

#' Show an activity
#' 
#' It can be used to visualise an activity's internal structure.
#'
#' @param activity an external pointer to the activity.
#' @seealso Other methods to deal with activities: \link{get_next_activity}
#' @export
show_activity <- function(activity) activity_show_(activity, 0)

#' Get the first activity
#'
#' It can be used to get the pointer to a trajectory's first activity.
#' 
#' @param traj the trajectory object.
#' @return An external pointer to an activity object.
#' @seealso Other methods to deal with trajectories:
#' \link{create_trajectory}, \link{show_trajectory}, \link{get_tail},
#' \link{get_n_activities}, \link{seize}, \link{release}, \link{timeout}, \link{branch}.
#' @export
get_head <- function(traj) traj$get_head()

#' Get the last activity
#'
#' It can be used to get the pointer to a trajectory's last activity.
#' 
#' @param traj the trajectory object.
#' @return An external pointer to an activity object.
#' @seealso Other methods to deal with trajectories:
#' \link{create_trajectory}, \link{show_trajectory}, \link{get_head},
#' \link{get_n_activities}, \link{seize}, \link{release}, \link{timeout}, \link{branch}.
#' @export
get_tail <- function(traj) traj$get_tail()

#' Get the next activity
#' 
#' It takes an external pointer to an activity an returns the next activity.
#'
#' @param activity an external pointer to the activity.
#' @return An external pointer to an activity object.
#' @seealso Other methods to deal with activities: \link{show_activity}
#' @export
get_next_activity <- function(activity) activity_get_next_(activity)

#' Get the number of activities
#'
#' It can be used to get the total number of activities defined inside a trajectory.
#' 
#' @param traj the trajectory object.
#' @return The number of activities in the trajectory.
#' @seealso Other methods to deal with trajectories:
#' \link{create_trajectory}, \link{show_trajectory}, \link{get_head},
#' \link{get_tail}, \link{seize}, \link{release}, \link{timeout}, \link{branch}.
#' @export
get_n_activities <- function(traj) traj$get_n_activities()

#' Add a seize activity
#'
#' Adds a new activity capable of seizing a resource to the tail of a trajectory.
#' 
#' @param traj the trajectory object.
#' @param resource the name of the resource.
#' @param amount the amount to seize.
#' @return The trajectory object.
#' @seealso Other methods to deal with trajectories:
#' \link{create_trajectory}, \link{show_trajectory}, \link{get_head},
#' \link{get_tail}, \link{get_n_activities}, \link{release}, \link{timeout}, \link{branch}.
#' @export
seize <- function(traj, resource, amount) traj$seize(resource, amount)

#' Add a release activity
#'
#' Adds a new activity capable of releasing a resource to the tail of a trajectory.
#' 
#' @param traj the trajectory object.
#' @param resource the name of the resource.
#' @param amount the amount to release.
#' @return The trajectory object.
#' @seealso Other methods to deal with trajectories:
#' \link{create_trajectory}, \link{show_trajectory}, \link{get_head},
#' \link{get_tail}, \link{get_n_activities}, \link{seize}, \link{timeout}, \link{branch}.
#' @export
release <- function(traj, resource, amount) traj$release(resource, amount)

#' Add a timeout activity
#'
#' Adds a new activity capable of executing any user-defined task and setting
#' an associated delay to the tail of a trajectory.
#' 
#' @param traj the trajectory object.
#' @param duration a callable object (a function) that returns a numeric value 
#' (negative values are automatically converted to positive ones)
#' @return The trajectory object.
#' @seealso Other methods to deal with trajectories:
#' \link{create_trajectory}, \link{show_trajectory}, \link{get_head},
#' \link{get_tail}, \link{get_n_activities}, \link{seize}, \link{release}, \link{branch}.
#' @export
timeout <- function(traj, duration) traj$timeout(duration)

#' Add a branch activity
#'
#' Adds a new activity that defines n alternative paths to the tail of a trajectory.
#' 
#' @param traj the trajectory object.
#' @param option a callable object (a function) that must return an integer 
#' between 1 and n; it will be used by the arrivals to select a path to follow.
#' @param merge a vector of n booleans that indicate whether the arrival must continue 
#' executing activities after each path or not.
#' @param ... n trajectory objects describing each path.
#' @return The trajectory object.
#' @seealso Other methods to deal with trajectories:
#' \link{create_trajectory}, \link{show_trajectory}, \link{get_head},
#' \link{get_tail}, \link{get_n_activities}, \link{seize}, \link{release}, \link{timeout}.
#' @export
branch <- function(traj, option, merge, ...) traj$branch(option, merge, ...)
