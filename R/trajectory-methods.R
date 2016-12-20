#' Create a trajectory
#'
#' This method initialises a trajectory object, which comprises a chain of
#' activities that can be attached to a generator.
#'
#' @param name the name of the trajectory.
#' @param verbose enable showing additional information.
#'
#' @return Returns an environment that represents the trajectory.
#' @seealso Methods for dealing with trajectories:
#' \code{\link{[.trajectory}}, \code{\link{[[.trajectory}}, \code{\link{length.trajectory}},
#' \code{\link{get_n_activities}}, \code{\link{join}}, \code{\link{seize}}, \code{\link{release}},
#' \code{\link{seize_selected}}, \code{\link{release_selected}}, \code{\link{select}},
#' \code{\link{set_capacity}}, \code{\link{set_queue_size}}, \code{\link{set_capacity_selected}},
#' \code{\link{set_queue_size_selected}}, \code{\link{set_prioritization}}, \code{\link{activate}},
#' \code{\link{deactivate}}, \code{\link{set_trajectory}}, \code{\link{set_distribution}},
#' \code{\link{set_attribute}}, \code{\link{timeout}}, \code{\link{branch}}, \code{\link{rollback}},
#' \code{\link{leave}}, \code{\link{renege_in}}, \code{\link{renege_if}}, \code{\link{renege_abort}},
#' \code{\link{clone}}, \code{\link{synchronize}}, \code{\link{batch}}, \code{\link{separate}},
#' \code{\link{send}}, \code{\link{trap}}, \code{\link{untrap}}, \code{\link{wait}}, \code{\link{log_}}.
#' @export
#'
#' @examples
#' t0 <- trajectory("my trajectory") %>%
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
#' t1 <- trajectory("trajectory with a branch") %>%
#'   seize("server", 1) %>%
#'   # 50-50 chance for each branch
#'   branch(function() sample(1:2, 1), continue=c(TRUE, FALSE),
#'     trajectory("branch1") %>%
#'       timeout(function() 1),
#'     trajectory("branch2") %>%
#'       timeout(function() rexp(1, 3)) %>%
#'       release("server", 1)
#'   ) %>%
#'   # only the first branch continues here
#'   release("server", 1) %>%
#'   timeout(function() 2)
#'
#' t1
trajectory <- function(name="anonymous", verbose=FALSE) Trajectory$new(name, verbose)

#' @rdname trajectory
#' @export
create_trajectory <- function(name="anonymous", verbose=FALSE) { # nocov start
  .Deprecated("trajectory")
  trajectory(name, verbose)
} # nocov end

#' Extract parts of a trajectory
#'
#' Operators acting on trajectories.
#'
#' @param x the trajectory object.
#' @param i indices specifying elements to extract. Indices are \code{numeric} or \code{character}
#' or \code{logical} vectors or empty (missing) or \code{NULL}.
#'
#' Numeric values are coerced to integer as by \code{\link{as.integer}} (and hence truncated towards
#' zero). Negative integers indicate elemets/slices to leave out the selection.
#'
#' Character vectors will be matched to the names of the activities in the trajectory as by
#' \code{\link{\%in\%}}.
#'
#' Logical vectors indicate elements/slices to select. Such vectors are NOT recycled to match the
#' corresponding extent.
#'
#' An empty index will return the whole trajectory.
#'
#' An index value of \code{NULL} is treated as if it were \code{integer(0)}.
#'
#' @return Returns a new trajectory object.
#' @seealso \code{\link{length.trajectory}}, \code{\link{get_n_activities}}, \code{\link{join}}.
#'
#' @name Extract
#' @export
`[.trajectory` <- function(x, i) x$subset(i)

#' @rdname Extract
#' @export
`[[.trajectory` <- function(x, i) {
  stopifnot(length(i) == 1L)
  stopifnot(is.character(i) | (is.numeric(i) & i > 0))
  x$subset(i)$subset(1)
}

#' Number of activities in a trajectory
#'
#' Get the number of activities in a trajectory. \code{length} returns the number
#' of first-level activities (sub-trajectories not included). \code{get_n_activities}
#' returns the total number of activities (sub-trajectories included).
#'
#' @inheritParams Extract
#'
#' @return Returns a non-negative integer of length 1.
#' @seealso \code{\link{[.trajectory}}, \code{\link{[[.trajectory}}, \code{\link{join}}.
#'
#' @name length
#' @export
length.trajectory <- function(x) x$length()

#' @rdname length
#' @export
get_n_activities <- function(x) UseMethod("get_n_activities")

#' @export
get_n_activities.trajectory <- function(x) x$get_n_activities()

#' Join trajectories
#'
#' Concatenate any number of trajectories in the specified order.
#'
#' @param ... trajectory objects.
#'
#' @return Returns a new trajectory object.
#' @seealso \code{\link{[.trajectory}}, \code{\link{[[.trajectory}}, \code{\link{length.trajectory}},
#' \code{\link{get_n_activities}}.
#' @export
#'
#' @examples
#' t1 <- trajectory() %>% seize("dummy", 1)
#' t2 <- trajectory() %>% timeout(1)
#' t3 <- trajectory() %>% release("dummy", 1)
#'
#' join(t1, t2, t3)
#'
#' trajectory() %>%
#'   join(t1) %>%
#'   timeout(1) %>%
#'   join(t3)
join <- function(...) UseMethod("join", c(...)[[1]])

#' @export
join.trajectory <- function(...) {
  traj <- c(...)
  for (i in traj[-1]) traj[[1]] <- traj[[1]]$join(i)
  traj[[1]]
}
