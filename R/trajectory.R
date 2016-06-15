#' @importFrom R6 R6Class
#' @importFrom Rcpp evalCpp
simmer.trajectory <- R6Class("simmer.trajectory",
  public = list(
    name = NA,
    
    initialize = function(name="anonymous", verbose=FALSE) { 
      self$name <- evaluate_value(name)
      private$verbose <- evaluate_value(verbose)
      self
    },
    
    print = function(indent=0) {
      margin <- paste(rep(" ", indent), collapse="")
      cat(paste0(margin, "simmer trajectory: ", self$name, ", ",
                 private$n_activities, " activities\n"))
      ptr <- private$head
      while (!identical(ptr, private$tail)) {
        activity_print_(ptr, indent)
        ptr <- activity_get_next_(ptr)
      }
      if (!is.null(ptr)) activity_print_(ptr, indent)
    },
    
    get_head = function() { private$head },
    
    get_tail = function() { private$tail },
    
    get_n_activities = function() { private$n_activities },
    
    seize = function(resource, amount=1, priority=0, preemptible=0, restart=FALSE) {
      resource <- evaluate_value(resource)
      amount <- evaluate_value(amount)
      priority <- evaluate_value(priority)
      preemptible <- evaluate_value(preemptible)
      if (preemptible < priority) {
        warning("`preemptible` value has changed from ", preemptible, " to ", priority,
                " (see ?seize for more details)")
        preemptible <- priority
      }
      restart <- evaluate_value(restart)
      if (is.function(amount))
        private$add_activity(Seize__new_func(private$verbose, resource, amount, 
                                             needs_attrs(amount), priority, preemptible, restart))
      else private$add_activity(Seize__new(private$verbose, resource, amount, 
                                           priority, preemptible, restart))
    },
    
    release = function(resource, amount=1) {
      resource <- evaluate_value(resource)
      amount <- evaluate_value(amount)
      if (is.function(amount))
        private$add_activity(Release__new_func(private$verbose, resource, amount, needs_attrs(amount)))
      else private$add_activity(Release__new(private$verbose, resource, amount))
    },
    
    timeout = function(task) {
      task <- evaluate_value(task)
      if (is.function(task))
        private$add_activity(Timeout__new_func(private$verbose, task, needs_attrs(task)))
      else private$add_activity(Timeout__new(private$verbose, task))
    },
    
    set_attribute = function(key, value) {
      key <- as.character(key)
      value <- evaluate_value(value)
      if (is.function(value))
        private$add_activity(SetAttribute__new_func(private$verbose, key, value, needs_attrs(value)))
      else private$add_activity(SetAttribute__new(private$verbose, key, value))
    },
    
    branch = function(option, continue, ...) {
      trj <- list(...)
      if (length(continue) != length(trj))
        stop("the number of elements does not match")
      for (i in trj) if (!inherits(i, "simmer.trajectory"))
        stop("not a trajectory")
      private$add_activity(Branch__new(private$verbose, option, needs_attrs(option), continue, trj))
    },
    
    rollback = function(amount, times=1, check) {
      amount <- evaluate_value(amount)
      times <- evaluate_value(times)
      if (is.infinite(times)) times <- -1
      if (missing(check))
        private$add_activity(Rollback__new(private$verbose, amount, times))
      else private$add_activity(Rollback__new_func(private$verbose, amount, check, needs_attrs(check)))
    },
    
    join = function(traj) {
      if (!inherits(traj, "simmer.trajectory"))
        stop("not a trajectory")
      new <- self$clone(deep=TRUE)
      traj <- traj$clone(deep=TRUE)
      if (!is.null(traj$get_head())) {
        if (!is.null(new$get_tail()))
          activity_chain_(new$get_tail(), traj$get_head())
        else new$.__enclos_env__$private$head <- traj$get_head()
        new$.__enclos_env__$private$tail <- traj$get_tail()
      }
      new$.__enclos_env__$private$n_activities <- 
        new$.__enclos_env__$private$n_activities + traj$get_n_activities()
      new
    }
  ),
  
  private = list(
    verbose = FALSE,
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
      self
    },
    
    clone2 = function(){},
    copy = function(deep = FALSE) {
      new <- private$clone2(deep)
      if (!is.null(new$get_head())) {
        ptr <- new$get_head()
        first <- activity_clone_(ptr)
        last <- first
        while (!identical(ptr, new$get_tail())) {
          ptr <- activity_get_next_(ptr)
          new_ptr <- activity_clone_(ptr)
          activity_chain_(last, new_ptr)
          last <- new_ptr
        }
        new$.__enclos_env__$private$head <- first
        new$.__enclos_env__$private$tail <- last
      }
      new
    }
  )
)
simmer.trajectory$private_methods$clone2 <- simmer.trajectory$public_methods$clone
simmer.trajectory$public_methods$clone <- simmer.trajectory$private_methods$copy

#' Create a trajectory
#'
#' This function initialises a trajectory, which is a chain of activities followed 
#' by arrivals of the same type.
#' 
#' @param name the name of the trajectory.
#' @param verbose enable showing additional information.
#' 
#' @return Returns an environment that represents the trajectory.
#' @seealso Other methods for dealing with trajectories:
#' \link{get_head}, \link{get_tail}, \link{get_n_activities}, 
#' \link{join}, \link{seize}, \link{release}, \link{timeout}, 
#' \link{set_attribute}, \link{branch}, \link{rollback}.
#' @export
#' 
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
#' t0
#' 
#' t1 <- create_trajectory("trajectory with a branch") %>%
#'   seize("server", 1) %>%
#'   ## 50-50 chance for each branch
#'   branch(function() sample(1:2, 1), continue=c(TRUE, FALSE), 
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
#' t1
create_trajectory <- function(name="anonymous", verbose=FALSE) simmer.trajectory$new(name, verbose)

#' Get the first activity
#'
#' It can be used to get the pointer to a trajectory's first activity.
#' 
#' @param traj the trajectory object.
#' 
#' @return An external pointer to an activity object.
#' @seealso Other methods for dealing with trajectories:
#' \link{create_trajectory}, \link{get_tail}, \link{get_n_activities}, 
#' \link{join}, \link{seize}, \link{release}, \link{timeout}, 
#' \link{set_attribute}, \link{branch}, \link{rollback}.
#' @export
get_head <- function(traj) traj$get_head()

#' Get the last activity
#'
#' It can be used to get the pointer to a trajectory's last activity.
#' 
#' @param traj the trajectory object.
#' 
#' @return An external pointer to an activity object.
#' @seealso Other methods for dealing with trajectories:
#' \link{create_trajectory}, \link{get_head}, \link{get_n_activities}, 
#' \link{join}, \link{seize}, \link{release}, \link{timeout}, 
#' \link{set_attribute}, \link{branch}, \link{rollback}.
#' @export
get_tail <- function(traj) traj$get_tail()

#' Get the number of activities
#'
#' It can be used to get the total number of activities defined inside a trajectory.
#' 
#' @param traj the trajectory object.
#' 
#' @return The number of activities in the trajectory.
#' @seealso Other methods for dealing with trajectories:
#' \link{create_trajectory}, \link{get_head}, \link{get_tail}, 
#' \link{join}, \link{seize}, \link{release}, \link{timeout}, 
#' \link{set_attribute}, \link{branch}, \link{rollback}.
#' @export
get_n_activities <- function(traj) traj$get_n_activities()

#' Join trajectories
#'
#' Concatenate any number of trajectories in the order specified.
#' 
#' @param ... trajectory objects.
#' 
#' @return A new trajectory object.
#' @seealso Other methods for dealing with trajectories:
#' \link{create_trajectory}, \link{get_head}, \link{get_tail},
#' \link{get_n_activities}, \link{seize}, \link{release}, \link{timeout}, 
#' \link{set_attribute}, \link{branch}, \link{rollback}.
#' @export
#' 
#' @examples
#' t1 <- create_trajectory() %>% seize("dummy", 1)
#' t2 <- create_trajectory() %>% timeout(1)
#' t3 <- create_trajectory() %>% release("dummy", 1)
#' 
#' join(t1, t2, t3)
#' 
#' create_trajectory() %>%
#'   join(t1) %>%
#'   timeout(1) %>%
#'   join(t3)
join <- function(...) {
  traj <- c(...)
  for (i in traj[-1]) traj[[1]] <- traj[[1]]$join(i)
  traj[[1]]
}

#' Add a seize activity
#'
#' Adds a new activity capable of seizing a resource to the tail of a trajectory.
#' 
#' @param traj the trajectory object.
#' @param resource the name of the resource.
#' @param amount the amount to seize, accepts either a callable object (a function) or a numeric value.
#' @param priority the priority of the seize (a higher integer equals higher 
#' priority; defaults to the minimum priority, which is 0).
#' @param preemptible if the seize occurs in a preemptive resource, this parameter 
#' establishes the minimum incoming priority that can preempt this arrival (a seize 
#' with a priority greater than `preemptible` gains the resource). In any case,
#' `preemptible` must be equal or greater than `priority`, and thus only higher
#' priority seizes can trigger the preemption.
#' @param restart whether the activity must be restarted after being preempted.
#' 
#' @return The trajectory object.
#' @seealso Other methods for dealing with trajectories:
#' \link{create_trajectory}, \link{get_head}, \link{get_tail}, 
#' \link{get_n_activities}, \link{join}, \link{release}, \link{timeout}, 
#' \link{set_attribute}, \link{branch}, \link{rollback}.
#' @export
seize <- function(traj, resource, amount=1, priority=0, preemptible=0, restart=FALSE)
  traj$seize(resource, amount, priority, preemptible, restart)

#' Add a release activity
#'
#' Adds a new activity capable of releasing a resource to the tail of a trajectory.
#' 
#' @param traj the trajectory object.
#' @param resource the name of the resource.
#' @param amount the amount to release, accepts either a callable object (a function) or a numeric value.
#' 
#' @return The trajectory object.
#' @seealso Other methods for dealing with trajectories:
#' \link{create_trajectory}, \link{get_head}, \link{get_tail}, 
#' \link{get_n_activities}, \link{join}, \link{seize}, \link{timeout}, 
#' \link{set_attribute}, \link{branch}, \link{rollback}.
#' @export
release <- function(traj, resource, amount=1) traj$release(resource, amount)

#' Add a timeout activity
#'
#' Adds a new activity capable of executing any user-defined task and setting
#' an associated delay to the tail of a trajectory.
#' 
#' @param traj the trajectory object.
#' @param task the timeout duration supplied by either passing a numeric value or a callable object (a function) that returns a numeric value 
#' (negative values are automatically coerced to positive).
#' 
#' @return The trajectory object.
#' @seealso Other methods for dealing with trajectories:
#' \link{create_trajectory}, \link{get_head}, \link{get_tail}, 
#' \link{get_n_activities}, \link{join}, \link{seize}, \link{release}, 
#' \link{set_attribute}, \link{branch}, \link{rollback}.
#' @export
timeout <- function(traj, task) traj$timeout(task)

#' Add a set attribute activity
#'
#' Adds a new key/value attribute. The value should be numeric.
#' 
#' @param traj the trajectory object.
#' @param key the attribute key (is coerced to a string).
#' @param value the value to set, accepts either a numeric or a callable object (a function) which returns a numeric.
#' 
#' @return The trajectory object.
#' @seealso Other methods for dealing with trajectories:
#' \link{create_trajectory}, \link{get_head}, \link{get_tail}, 
#' \link{get_n_activities}, \link{join}, \link{seize}, \link{release}, 
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
#' @param continue a vector of \code{n} booleans that indicate whether the arrival must 
#' continue executing activities after each path or not.
#' @param merge (deprecated) same as \code{continue}, for compatibility reasons. Support
#' for this argument will be dropped in upcoming versions.
#' @param ... \code{n} trajectory objects describing each path.
#' 
#' @return The trajectory object.
#' @seealso Other methods for dealing with trajectories:
#' \link{create_trajectory}, \link{get_head}, \link{get_tail}, 
#' \link{get_n_activities}, \link{join}, \link{seize}, \link{release}, 
#' \link{set_attribute}, \link{timeout}, \link{rollback}.
#' @export
branch <- function(traj, option, continue, ..., merge="_deprecated") {
  if(!identical(merge, "_deprecated")) {
    warning("'merge' is deprecated, use 'continue' instead") # nocov
    traj$branch(option, merge, continue, ...) # nocov
  } else traj$branch(option, continue, ...)
}

#' Add a rollback activity
#'
#' Adds a new activity that goes backwards in the trajectory.
#' 
#' @param traj the trajectory object.
#' @param amount the amount of activities (of the same or parent trajectories) to roll back.
#' @param times the number of repetitions until an arrival may continue.
#' @param check a callable object (a function) that must return a boolean. If
#' present, the \code{times} parameter is ignored, and the activity uses this
#' function to check whether the rollback must be done or not.
#' 
#' @return The trajectory object.
#' @seealso Other methods for dealing with trajectories:
#' \link{create_trajectory}, \link{get_head}, \link{get_tail}, 
#' \link{get_n_activities}, \link{join}, \link{seize}, \link{release},
#' \link{set_attribute}, \link{timeout}, \link{branch}.
#' @export
rollback <- function(traj, amount, times=1, check) traj$rollback(amount, times, check)
