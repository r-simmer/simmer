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

#' Convenience Functions for Generators
#'
#' These convenience functions facilitate the definition of generators of arrivals
#' for some common cases.
#'
#' @return Returns a generator function (a closure).
#' @seealso \code{\link{add_generator}}.
#' @name generators
#' @examples
#' ## common to all examples below
#' # some trajectory
#' t0 <- trajectory() %>%
#'   timeout(0)
#' # some distribution
#' distr <- function() runif(1, 1, 2)
#'
NULL

#' @rdname generators
#' @param ... a vector or multiple parameters of times at which to initiate an arrival.
#'
#' @details \code{\link{at}} generates arrivals at specific absolute times.
#' @export
#' @examples
#' # arrivals at 0, 1, 10, 30, 40 and 43
#' simmer() %>%
#'   add_generator("dummy", t0, at(0, c(1,10,30), 40, 43)) %>%
#'   run(100) %>%
#'   get_mon_arrivals()
#'
at <- function(...) {
  time_vec <- c(...)
  time_diffs <- c(time_vec[1], diff(time_vec))
  function() return(c(time_diffs, -1))
}

#' @rdname generators
#' @param start_time the time at which to launch the initial arrival
#' (numeric or function).
#' @param dist a function modelling the interarrival times. It is supposed to be
#' an infinite source of values \code{>= 0} (e.g., \code{rexp} and the like). If
#' the function provided returns any negative value, the behaviour is undefined.
#' @param arrive if set to \code{TRUE} (default) the first arrival will be
#' generated at \code{start_time} and will follow \code{dist} from then on.
#' If set to \code{FALSE}, will initiate \code{dist} at \code{start_time}
#' (and the first arrival will most likely start at a time later than
#' \code{start_time}).
#'
#' @details \code{\link{from}} generates inter-arrivals following a given distribution
#' with a specified start time.
#' union of the last two.
#' @export
#' @examples
#' # apply distribution starting at 5 (and no end)
#' simmer() %>%
#'   add_generator("dummy", t0, from(5, distr)) %>%
#'   run(10) %>%
#'   get_mon_arrivals()
#'
from <- function(start_time, dist, arrive=TRUE) {
  replace_env(start_time, dist)
  .started <- FALSE
  function() {
    if (!.started) {
      .started <<- TRUE
      if (arrive) {
        dt <- getval(start_time)
      } else {
        dt <- dist()
        if (dt[1] >= 0)
          dt[1] <- dt[1] + getval(start_time)
      }
    } else {
      dt <- dist()
    }
    dt
  }
}

#' @rdname generators
#' @param stop_time the time at which to stop the generator (numeric or function).
#'
#' @details \code{\link{to}} generates inter-arrivals following a given
#' distribution with a specified stop time.
#' @export
#' @examples
#' # apply distribution until 5 (starting at 0)
#' simmer() %>%
#'   add_generator("dummy", t0, to(5, distr)) %>%
#'   run(10) %>%
#'   get_mon_arrivals()
#'
to <- function(stop_time, dist) {
  replace_env(stop_time, dist)
  .counter <- 0
  function() {
    dt <- dist()
    len <- length(dt)
    dt <- dt[cumsum(dt) + .counter < getval(stop_time)]
    .counter <<- .counter + sum(dt)
    if (len == length(dt)) return(dt)
    return(c(dt, -1))
  }
}

#' @rdname generators
#' @param every repeat with this time cycle (numeric or function).
#'
#' @details \code{\link{from_to}} is the union of \code{from} and \code{to}.
#' @export
#' @examples
#' # apply distribution from 8 to 16 h every 24 h:
#' simmer() %>%
#'   add_generator("dummy", t0, from_to(8, 16, distr, every=24)) %>%
#'   run(48) %>%
#'   get_mon_arrivals()
#'
from_to <- function(start_time, stop_time, dist, arrive=TRUE, every=NULL) {
  replace_env(start_time, stop_time, every, dist)
  .started <- FALSE
  .init <- 0
  .counter <- 0
  .every <- 0
  function() {
    while (TRUE){
      if (!.started) {
        .started <<- TRUE
        if (arrive) {
          dt <- getval(start_time) + .every - .init
        } else {
          dt <- dist()
          if (dt[1] >= 0)
            dt[1] <- dt[1] + getval(start_time) + .every - .init
        }
      } else {
        dt <- dist()
      }
      len <- length(dt)
      dt <- dt[cumsum(dt) + .counter < getval(stop_time) + .every]
      .counter <<- .counter + sum(dt)
      if (len == length(dt))
        return(dt)
      if (is.null(every))
        return(c(dt, -1))
      if (length(dt))
        return(dt)
      .started <<- FALSE
      .init <<- .counter
      .every <<- .every + getval(every)
    }
  }
}

#' @rdname generators
#' @param n an integer or a callable object (a function) which must return
#' a number of arrivals to generate when activated.
#'
#' @details \code{\link{when_activated}} sets up an initially inactive generator
#' which generates \code{n} arrivals each time it is activated from any
#' trajectory using the activity \code{\link{activate}}.
#' @export
#' @examples
#' # triggering arrivals on demand from a trajectory
#' t1 <- trajectory() %>%
#'   activate("dummy")
#'
#' simmer() %>%
#'   add_generator("dummy", t0, when_activated()) %>%
#'   add_generator("trigger", t1, at(2)) %>%
#'   run() %>%
#'   get_mon_arrivals()
#'
when_activated <- function(n=1) {
  first <- TRUE
  function() {
    if (first) {
      first <<- FALSE
      return(-1)
    }
    c(rep(0, if (is.function(n)) n() else n), -1)
  }
}
