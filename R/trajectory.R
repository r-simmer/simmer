#' @importFrom R6 R6Class
#' @importFrom Rcpp evalCpp
Trajectory <- R6Class("Trajectory",
  public = list(
    name = NA,
    
    initialize = function(name="anonymous") { 
      self$name <- evaluate_value(name)
      invisible(self)
    },
    
    print = function() self$show(),
    
    show = function(indent=0) {
      margin <- paste(rep(" ", indent), collapse="")
      cat(paste0(margin, "Trajectory: ", self$name, ", ",
                 private$n_activities, " activities\n"))
      ptr <- private$head
      while (!identical(ptr, private$tail)) {
        activity_show_(ptr, indent)
        ptr <- activity_get_next_(ptr)
      }
      if (!is.null(ptr)) activity_show_(ptr, indent)
    },
    
    get_head = function() { private$head },
    
    get_tail = function() { private$tail },
    
    get_n_activities = function() { private$n_activities },
    
    seize = function(resource, amount=1, priority=0) {
      resource <- evaluate_value(resource)
      amount <- evaluate_value(amount)
      if (is.function(amount))
        private$add_activity(Seize__new_func(resource, amount, needs_attrs(amount), priority))
      else private$add_activity(Seize__new(resource, amount, priority))
    },
    
    release = function(resource, amount=1) {
      resource <- evaluate_value(resource)
      amount <- evaluate_value(amount)
      if (is.function(amount))
        private$add_activity(Release__new_func(resource, amount, needs_attrs(amount)))
      else private$add_activity(Release__new(resource, amount))
    },
    
    timeout = function(task) {
      task <- evaluate_value(task)
      if (is.function(task))
        private$add_activity(Timeout__new_func(task, needs_attrs(task)))
      else private$add_activity(Timeout__new(task))
    },
    
    set_attribute = function(key, value) {
      key <- as.character(key)
      value <- evaluate_value(value)
      if (is.function(value))
        private$add_activity(SetAttribute__new_func(key, value, needs_attrs(value)))
      else private$add_activity(SetAttribute__new(key, value))
    },
    
    branch = function(option, merge, ...) {
      trj <- list(...)
      if (length(merge) != length(trj))
        stop("the number of elements does not match")
      for (i in trj) if (!inherits(i, "Trajectory"))
        stop("not a trajectory")
      private$add_activity(Branch__new(option, merge, trj))
    },
    
    rollback = function(amount, times=1, check) {
      amount <- evaluate_value(amount)
      times <- evaluate_value(times)
      if (is.infinite(times)) times <- -1
      if (missing(check))
        private$add_activity(Rollback__new(amount, times))
      else private$add_activity(Rollback__new_func(amount, check, needs_attrs(check)))
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
        activity_chain_(private$tail, activity)
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
#' \link{get_n_activities}, \link{seize}, \link{release}, \link{timeout}, 
#' \link{set_attribute}, \link{branch}, \link{rollback}.
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
#' \link{get_n_activities}, \link{seize}, \link{release}, \link{timeout}, 
#' \link{set_attribute}, \link{branch}, \link{rollback}.
#' @export
show_trajectory <- function(traj) traj$show()

#' Get the first activity
#'
#' It can be used to get the pointer to a trajectory's first activity.
#' 
#' @param traj the trajectory object.
#' @return An external pointer to an activity object.
#' @seealso Other methods to deal with trajectories:
#' \link{create_trajectory}, \link{show_trajectory}, \link{get_tail},
#' \link{get_n_activities}, \link{seize}, \link{release}, \link{timeout}, 
#' \link{set_attribute}, \link{branch}, \link{rollback}.
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
#' \link{get_n_activities}, \link{seize}, \link{release}, \link{timeout}, 
#' \link{set_attribute}, \link{branch}, \link{rollback}.
#' @export
get_tail <- function(traj) traj$get_tail()

#' Get the number of activities
#'
#' It can be used to get the total number of activities defined inside a trajectory.
#' 
#' @param traj the trajectory object.
#' @return The number of activities in the trajectory.
#' @seealso Other methods to deal with trajectories:
#' \link{create_trajectory}, \link{show_trajectory}, \link{get_head},
#' \link{get_tail}, \link{seize}, \link{release}, \link{timeout}, 
#' \link{set_attribute}, \link{branch}, \link{rollback}.
#' @export
get_n_activities <- function(traj) traj$get_n_activities()

#' Add a seize activity
#'
#' Adds a new activity capable of seizing a resource to the tail of a trajectory.
#' 
#' @param traj the trajectory object.
#' @param resource the name of the resource.
#' @param amount the amount to seize.
#' @oaram the priority of the seize (a higher integer equals higher priority, defaults to 0)
#' @return The trajectory object.
#' @seealso Other methods to deal with trajectories:
#' \link{create_trajectory}, \link{show_trajectory}, \link{get_head},
#' \link{get_tail}, \link{get_n_activities}, \link{release}, \link{timeout}, 
#' \link{set_attribute}, \link{branch}, \link{rollback}.
#' @export
seize <- function(traj, resource, amount=1, priority=0) traj$seize(resource, amount, priority)

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
#' \link{get_tail}, \link{get_n_activities}, \link{seize}, \link{timeout}, 
#' \link{set_attribute}, \link{branch}, \link{rollback}.
#' @export
release <- function(traj, resource, amount=1) traj$release(resource, amount)

#' Add a timeout activity
#'
#' Adds a new activity capable of executing any user-defined task and setting
#' an associated delay to the tail of a trajectory.
#' 
#' @param traj the trajectory object.
#' @param task a callable object (a function) that returns a numeric value 
#' (negative values are automatically coerced to positive).
#' @return The trajectory object.
#' @seealso Other methods to deal with trajectories:
#' \link{create_trajectory}, \link{show_trajectory}, \link{get_head},
#' \link{get_tail}, \link{get_n_activities}, \link{seize}, \link{release}, 
#' \link{set_attribute}, \link{branch}, \link{rollback}.
#' @export
timeout <- function(traj, task) traj$timeout(task)

#' Add a set attribute activity
#'
#' Adds a new key/value attribute. The value should be numeric.
#' 
#' @param traj the trajectory object.
#' @param key the attribute key (is coerced to a string).
#' @param value the value (should be numeric or a function which returns a numeric).
#' @return The trajectory object.
#' @seealso Other methods to deal with trajectories:
#' \link{create_trajectory}, \link{show_trajectory}, \link{get_head},
#' \link{get_tail}, \link{get_n_activities}, \link{seize}, \link{release}, 
#' \link{timeout}, \link{branch}, \link{rollback}.
#' @export
set_attribute <- function(traj, key, value) traj$set_attribute(key, value)


#' Add a branch activity
#'
#' Adds a new activity that defines n alternative paths to the tail of a trajectory.
#' 
#' @param traj the trajectory object.
#' @param option a callable object (a function) that must return an integer 
#' between 1 and \code{n}; it will be used by the arrivals to select a path to follow.
#' @param merge a vector of \code{n} booleans that indicate whether the arrival must 
#' continue executing activities after each path or not.
#' @param ... \code{n} trajectory objects describing each path.
#' @return The trajectory object.
#' @seealso Other methods to deal with trajectories:
#' \link{create_trajectory}, \link{show_trajectory}, \link{get_head},
#' \link{get_tail}, \link{get_n_activities}, \link{seize}, \link{release}, 
#' \link{set_attribute}, \link{timeout}, \link{rollback}.
#' @export
branch <- function(traj, option, merge, ...) traj$branch(option, merge, ...)

#' Add a rollback activity
#'
#' Adds a new activity that goes backwards in the trajectory.
#' 
#' @param traj the trajectory object.
#' @param amount the amount of activities to roll back (of the same level; it does
#' not go into branches).
#' @param times the number of repetitions until an arrival may continue.
#' @param check a callable object (a function) that must return a boolean. If
#' present, the \code{times} parameter is ignored, and the activity uses this
#' function to check whether the rollback must be done or not.
#' @return The trajectory object.
#' @seealso Other methods to deal with trajectories:
#' \link{create_trajectory}, \link{show_trajectory}, \link{get_head},
#' \link{get_tail}, \link{get_n_activities}, \link{seize}, \link{release},
#' \link{set_attribute}, \link{timeout}, \link{branch}.
#' @export
rollback <- function(traj, amount, times=1, check) traj$rollback(amount, times, check)
