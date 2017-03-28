#' Arrivals at specific times
#'
#' Generator convenience function to generate arrivals at specific times.
#'
#' @param ... a vector or multiple parameters of times at which to initiate an arrival.
#'
#' @return Returns a generator function.
#' @seealso \code{\link{add_generator}}.
#' @export
#'
#' @examples
#' t0 <- trajectory() %>%
#'   timeout(0)
#'
#' simmer() %>%
#'   add_generator("dummy", t0, at(0, c(1,10,30), 40, 43)) %>%
#'   run(100) %>%
#'   get_mon_arrivals()
at <- function(...) {
  time_vec <- c(...)
  time_diffs <- c(time_vec[1], diff(time_vec))
  function() return(c(time_diffs, -1))
}

#' Generate arrivals starting at a specified time
#'
#' Generator convenience function to generate inter-arrivals with a specified start time.
#'
#' @param start_time the time at which to launch the initial arrival.
#' @param dist a function modelling the interarrival times.
#' @param arrive if set to \code{TRUE} (default) the first arrival will be
#' generated at \code{start_time} and will follow \code{dist} from then on.
#' If set to \code{FALSE}, will initiate \code{dist} at \code{start_time}
#' (and the first arrival will most likely start at a time later than
#' \code{start_time}).
#'
#' @return Returns a generator function.
#' @seealso \code{\link{add_generator}}.
#' @export
#'
#' @examples
#' t0 <- trajectory() %>%
#'   timeout(0)
#'
#' simmer() %>%
#'   add_generator("dummy", t0, from(5, function() runif(1, 1, 2))) %>%
#'   run(10) %>%
#'   get_mon_arrivals()
from <- function(start_time, dist, arrive=TRUE) {
  started <- FALSE
  function() {
    if (!started) {
      started <<- TRUE
      if (arrive) {
        return(start_time)
      } else {
        return(start_time + dist())
      }
    } else {
      return(dist())
    }
  }
}

#' Generate arrivals stopping at a specified time
#'
#' Generator convenience function to generate inter-arrivals with a specified stop time.
#'
#' @param stop_time the time at which to stop the generator.
#' @inheritParams from
#'
#' @return Returns a generator function.
#' @seealso \code{\link{add_generator}}.
#' @export
#'
#' @examples
#' t0 <- trajectory() %>%
#'   timeout(0)
#'
#' simmer() %>%
#'   add_generator("dummy", t0, to(5, function() runif(1, 1, 2))) %>%
#'   run(10) %>%
#'   get_mon_arrivals()
to <- function(stop_time, dist) {
  counter <- 0
  function() {
    dt <- dist()
    len <- length(dt)
    dt <- dt[cumsum(dt) + counter < stop_time]
    counter <<- counter + sum(dt)
    if (len == length(dt)) return(dt)
    return(c(dt, -1))
  }
}

#' Generate arrivals starting and stopping at specified times
#'
#' Generator convenience function to generate inter-arrivals with specified start and stop times.
#'
#' @inheritParams from
#' @inheritParams to
#' @param every repeat with this time cycle.
#'
#' @return Returns a generator function.
#' @seealso \code{\link{add_generator}}.
#' @export
#'
#' @examples
#' t0 <- trajectory() %>%
#'   timeout(0)
#'
#' # from 8 to 16 h every 24 h:
#' simmer() %>%
#'   add_generator("dummy", t0, from_to(8, 16, function() runif(1, 1, 2), every=24)) %>%
#'   run(48) %>%
#'   get_mon_arrivals()
from_to <- function(start_time, stop_time, dist, arrive=TRUE, every=NULL) {
  stopifnot(is.null(every) || every >= stop_time)
  started <- FALSE
  init <- 0
  counter <- 0
  function() {
    while (TRUE){
      if (!started) {
        started <<- TRUE
        if (arrive) {
          dt <- start_time - init
        } else {
          dt <- dist()
          dt[1] <- dt[1] + start_time - init
        }
      } else {
        dt <- dist()
      }
      len <- length(dt)
      dt <- dt[cumsum(dt) + counter < stop_time]
      counter <<- counter + sum(dt)
      if (len == length(dt))
        return(dt)
      if (is.null(every))
        return(c(dt, -1))
      if (length(dt))
        return(dt)
      start_time <<- start_time + every
      stop_time <<- stop_time + every
      started <<- FALSE
      init <<- counter
    }
  }
}
