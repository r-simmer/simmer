# Copyright (C) 2014-2015 Bart Smeets
# Copyright (C) 2015-2016 Bart Smeets and Iñaki Ucar
# Copyright (C) 2016-2019 Iñaki Ucar
#
# This file is part of simmer.
#
# simmer is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# simmer is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with simmer. If not, see <http://www.gnu.org/licenses/>.

#' Create a Trajectory
#'
#' This method initialises a trajectory object, which comprises a chain of
#' activities that can be attached to a generator. See below for a complete list
#' of available activities by category.
#'
#' @param name the name of the trajectory.
#' @param verbose enable showing additional information.
#'
#' @return Returns an environment that represents the trajectory.
#' @seealso
#' Available activities by category:
#' \itemize{
#'
#' \item Debugging: \code{\link{log_}}, \code{\link{stop_if}}
#'
#' \item Delays: \code{\link{timeout}}, \code{\link{timeout_from_attribute}},
#' \code{\link{timeout_from_global}}
#'
#' \item Arrival properties: \code{\link{set_attribute}}, \code{\link{set_global}},
#' \code{\link{set_prioritization}}
#'
#' \item Interaction with resources: \code{\link{select}}, \code{\link{seize}},
#' \code{\link{release}}, \code{\link{release_all}}, \code{\link{seize_selected}},
#' \code{\link{release_selected}}, \code{\link{release_selected_all}},
#' \code{\link{set_capacity}}, \code{\link{set_queue_size}},
#' \code{\link{set_capacity_selected}}, \code{\link{set_queue_size_selected}}
#'
#' \item Interaction with generators: \code{\link{activate}}, \code{\link{deactivate}},
#' \code{\link{set_trajectory}}, \code{\link{set_source}}
#'
#' \item Branching: \code{\link{branch}}, \code{\link{clone}}, \code{\link{synchronize}}
#'
#' \item Loops: \code{\link{rollback}}
#'
#' \item Batching: \code{\link{batch}}, \code{\link{separate}}
#'
#' \item Asynchronous programming: \code{\link{send}}, \code{\link{trap}},
#' \code{\link{untrap}}, \code{\link{wait}}
#'
#' \item Reneging: \code{\link{leave}}, \code{\link{handle_unfinished}},
#' \code{\link{renege_in}}, \code{\link{renege_if}}, \code{\link{renege_abort}}
#'
#' }
#'
#' Manage trajectories:
#' \itemize{
#' \item Extract or Replace Parts of a Trajectory: \code{\link{Extract.trajectory}}
#' \item Join Trajectories: \code{\link{join}}
#' \item Number of Activities in a Trajectory: \code{\link{length.trajectory}},
#' \code{\link{get_n_activities}}
#' }
#' @export
#'
#' @examples
#' ## create an empty trajectory
#' x <- trajectory("my trajectory")
#' x
#'
#' ## add some activities by chaining them
#' x <- x %>%
#'   log_("here I am!") %>%
#'   timeout(5) %>%
#'   log_("leaving!")
#' x
#'
#' ## join trajectories
#' x <- join(x, x)
#'
#' ## extract and replace
#' x[c(3, 4)] <- x[2]
#' x
#'
trajectory <- function(name="anonymous", verbose=FALSE) {
  check_args(name="character", verbose="flag")

  env <- list2env(list(
    name=name, verbose=verbose, n_activities=0, names=NULL, ptrs=NULL))
  env$head <- function() env$ptrs[[1]]
  env$tail <- function() env$ptrs[[length(env)]]
  env$clone <- function() subset.trajectory(env)

  class(env) <- "trajectory"
  env
}

#' @export
print.trajectory <- function(x, indent=0, verbose=x$verbose, ...) {
  margin <- paste(rep(" ", indent), collapse = "")
  cat(paste0(margin, "trajectory: ", x$name, ", ",
             x$n_activities, " activities\n"))
  lapply(x$ptrs, function(i) activity_print_(i, indent, verbose))
  invisible(x)
}

add_activity <- function(x, activity) {
  if (!is.null(x$ptrs))
    activity_chain_(x$tail(), activity)
  x$ptrs <- c(x$ptrs, activity)
  x$names <- c(x$names, get_caller())
  x$n_activities <- x$n_activities + activity_get_count_(activity)
  x
}

get_parts <- function(x, i, double=FALSE) {
  if (missing(i)) {
    parts <- seq_len(length(x))
  } else {
    stopifnot(length(i) <= length(x))
    if (is.null(i)) i <- 0
    if (is.logical(i)) {
      parts <- which(rep_len(i, length(x)))
    } else if (is.character(i)) {
      parts <- which(x$names %in% i)
      if (double) parts <- parts[[1]]
    } else if (is.numeric(i)) {
      i <- i[!is.na(i)]
      if (any(i < 0) && any(i > 0))
        stop("only 0's may be mixed with negative subscripts")
      i <- as.integer(i)
      i <- i[i != 0]
      if (any(i < 0))
        parts <- seq_len(length(x))[i]
      else parts <- i
    } else stop("invalid subscript type '", typeof(i), "'")
  }
  parts
}

subset.trajectory <- function(x, i, double=FALSE) {
  new <- trajectory(x$name, x$verbose)
  parts <- get_parts(x, i, double)
  if (length(parts)) {
    new$ptrs <- sapply(parts, function(i) {
      new_ptr <- activity_clone_(x$ptrs[[i]])
      new$n_activities <- new$n_activities + activity_get_count_(new_ptr)
      new_ptr
    })
    mapply(activity_chain_, new$ptrs[-length(new$ptrs)], new$ptrs[-1])
    new$names <- x$names[parts]
  }
  new
}

split.trajectory <- function(x)
  lapply(seq_len(length(x)), function(i) subset.trajectory(x, i))

replace.trajectory <- function(x, i, value, double=FALSE) {
  stopifnot(inherits(value, "trajectory"))
  if (!length(x)) x
  else {
    parts <- get_parts(x, i, double)
    new <- split.trajectory(x)
    new[parts] <- split.trajectory(value)
    new <- join(new)
    new$verbose <- x$verbose
    new
  }
}

#' Extract or Replace Parts of a Trajectory
#'
#' Operators acting on trajectories to extract or replace parts.
#'
#' @param x the trajectory object.
#' @param i indices specifying elements to extract. Indices are \code{numeric}
#' or \code{character} or \code{logical} vectors or empty (missing) or \code{NULL}.
#'
#' Numeric values are coerced to integer as by \code{\link{as.integer}} (and
#' hence truncated towards zero). Negative integers indicate elements/slices to
#' leave out the selection.
#'
#' Character vectors will be matched to the names of the activities in the
#' trajectory as by \code{\link{\%in\%}}.
#'
#' Logical vectors indicate elements/slices to select. Such vectors are recycled
#' if necessary to match the corresponding extent.
#'
#' An empty index will return the whole trajectory.
#'
#' An index value of \code{NULL} is treated as if it were \code{integer(0)}.
#' @param value another trajectory object.
#'
#' @return Returns a new trajectory object.
#' @seealso \code{\link{length.trajectory}}, \code{\link{get_n_activities}},
#' \code{\link{join}}.
#'
#' @name Extract.trajectory
#' @export
#'
#' @examples
#' x <- join(lapply(1:12, function(i)
#'   trajectory() %>% timeout(i)
#' ))
#' x
#'
#' x[10]                 # the tenth element of x
#' x[-1]                 # delete the 1st element of x
#' x[c(TRUE, FALSE)]     # logical indexing
#' x[c(1, 5, 2, 12, 4)]  # numeric indexing
#' x[c(FALSE, TRUE)] <- x[c(TRUE, FALSE)] # replacing
#' x
#'
`[.trajectory` <- function(x, i) subset.trajectory(x, i)

#' @rdname Extract.trajectory
#' @export
`[[.trajectory` <- function(x, i) {
  stopifnot(length(i) == 1L)
  stopifnot(is.character(i) | (is.numeric(i) & i > 0))
  subset.trajectory(x, i, double=TRUE)
}

#' @rdname Extract.trajectory
#' @export
`[<-.trajectory` <- function(x, i, value) {
  stopifnot(inherits(value, "trajectory"))
  replace.trajectory(x, i, value)
}

#' @rdname Extract.trajectory
#' @export
`[[<-.trajectory` <- function(x, i, value) {
  stopifnot(length(i) == 1L)
  stopifnot(inherits(value, "trajectory"))
  stopifnot(length(value) == 1L)
  stopifnot(is.character(i) | (is.numeric(i) & i > 0))
  replace.trajectory(x, i, value, double=TRUE)
}

#' Number of Activities in a Trajectory
#'
#' Get the number of activities in a trajectory. \code{length} returns the number
#' of first-level activities (sub-trajectories not included). \code{get_n_activities}
#' returns the total number of activities (sub-trajectories included).
#'
#' @inheritParams Extract.trajectory
#'
#' @return Returns a non-negative integer of length 1.
#' @seealso \code{\link{Extract.trajectory}}, \code{\link{join}}.
#'
#' @export
#'
#' @examples
#' x <- trajectory() %>%
#'   timeout(1)
#'
#' x <- x %>%
#'   clone(2, x, x)
#' x
#'
#' ## length does not account for subtrajectories
#' length(x)
#' get_n_activities(x)
#'
length.trajectory <- function(x) length(x$ptrs)

#' @rdname length.trajectory
#' @export
get_n_activities <- function(x) UseMethod("get_n_activities")

#' @export
get_n_activities.trajectory <- function(x) x$n_activities

#' Join Trajectories
#'
#' Concatenate any number of trajectories in the specified order.
#'
#' @param ... trajectory objects.
#'
#' @return Returns a new trajectory object.
#' @seealso \code{\link{Extract.trajectory}}, \code{\link{length.trajectory}},
#' \code{\link{get_n_activities}}.
#' @export
#'
#' @examples
#' t1 <- trajectory() %>% seize("dummy", 1)
#' t2 <- trajectory() %>% timeout(1)
#' t3 <- trajectory() %>% release("dummy", 1)
#'
#' ## join can be used alone
#' join(t1, t2, t3)
#'
#' ## or can be chained in a trajectory definition
#' trajectory() %>%
#'   join(t1) %>%
#'   timeout(1) %>%
#'   join(t3)
#'
join <- function(...) UseMethod("join", c(...)[[1]])

#' @export
join.trajectory <- function(...) {
  traj <- c(...)
  new <- traj[[1]]$clone()

  for (i in traj[-1]) {
    stopifnot(inherits(i, "trajectory"))

    i <- i$clone()
    if (!is.null(new$tail()) && !is.null(i$head()))
      activity_chain_(new$tail(), i$head())

    new$ptrs <- c(new$ptrs, i$ptrs)
    new$names <- c(new$names, i$names)
    new$n_activities <- new$n_activities + i$n_activities
  }
  new
}

#' @export
rep.trajectory <- function(x, ...) join(rep(split.trajectory(x), ...))
