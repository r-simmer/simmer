#' Arrivals at specific times
#' 
#' Generator convenience function to generate arrivals at specific times.
#'
#' @param ... a vector or multiple parameters of times at which to initiate an arrival.
#'
#' @return Returns a generator function.
#' @seealso \link{add_generator}, \link{every}, \link{from}, 
#' \link{to}, \link{from_to}.
#' @export
#'
#' @examples
#' t0 <- create_trajectory() %>% timeout(0)
#' env <- simmer(verbose=T) %>%
#'   add_generator("dummy", t0, at(0, c(1,10,30), 40, 43)) %>%
#'   run(100)
at <- function(...) {
  time_vec <- unlist(list(...))
  time_diffs <- c(time_vec[1], diff(time_vec))
  i <- 0
  function() {
    if (i < length(time_diffs)) {
      i <<- i+1
      return(time_diffs[i])
    } else {
      return(-1)
    }
  }
}

#' Arrivals every specific interval
#' 
#' Generator convenience function to generate arrivals every specific interval.
#' When the generator reaches the last interval, it starts again from the first one.
#'
#' @param ... a vector or multiple parameters of intervals between arrivals.
#'
#' @return Returns a generator function.
#' @seealso \link{add_generator}, \link{at}, \link{from}, 
#' \link{to}, \link{from_to}.
#' @export
#'
#' @examples
#' t0 <- create_trajectory() %>% timeout(0)
#' env <- simmer(verbose=T) %>%
#'   add_generator("dummy", t0, every(1, 2, 1)) %>%
#'   run(10)
every <- function(...) {
  time_diffs <- unlist(list(...))
  i <- 0
  function() {
    if (i < length(time_diffs)) {
      i <<- i+1
    } else {
      i <<- 1
    }
    return(time_diffs[i])
  }
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
#' @seealso \link{add_generator}, \link{at}, \link{every}, 
#' \link{to}, \link{from_to}.
#' @export
#'
#' @examples
#' t0 <- create_trajectory() %>% timeout(0)
#' env <- simmer(verbose=T) %>%
#'   add_generator("dummy", t0, from(5, function() runif(1, 1, 2))) %>%
#'   run(10)
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
#' @param dist a function modelling the interarrival times.
#'
#' @return Returns a generator function.
#' @seealso \link{add_generator}, \link{at}, \link{every}, \link{from}, 
#' \link{from_to}.
#' @export
#'
#' @examples
#' t0 <- create_trajectory() %>% timeout(0)
#' env <- simmer(verbose=T) %>%
#'   add_generator("dummy", t0, to(5, function() runif(1, 1, 2))) %>%
#'   run(10)
to <- function(stop_time, dist) {
  counter <- 0
  function() {
    dt <- dist()
    counter <<- counter + dt
    if (counter < stop_time) {
      return(dt)
    } else {
      return(-1)
    }
  }
}

#' Generate arrivals starting and stopping at specified times
#' 
#' Generator convenience function to generate inter-arrivals with specified start and stop times.
#'
#' @param start_time the time at which to launch the initial arrival.
#' @param stop_time the time at which to stop the generator.
#' @param dist a function modelling the interarrival times.
#' @param arrive if set to \code{TRUE} (default) the first arrival will be 
#' generated at \code{start_time} and will follow \code{dist} from then on. 
#' If set to \code{FALSE}, will initiate \code{dist} at \code{start_time} 
#' (and the first arrival will most likely start at a time later than 
#' \code{start_time}).
#' 
#' @return Returns a generator function.
#' @seealso \link{add_generator}, \link{at}, \link{every}, \link{from}, 
#' \link{to}.
#' @export
#'
#' @examples
#' t0 <- create_trajectory() %>% timeout(0)
#' env <- simmer(verbose=T) %>%
#'   add_generator("dummy", t0, from_to(5, 10, function() runif(1, 1, 2))) %>%
#'   run(100)
from_to <- function(start_time, stop_time, dist, arrive=TRUE){
  started <- FALSE
  counter <- 0
  function() {
    if (!started) {
      started <<- TRUE
      if (arrive) {
        dt <- start_time
      } else {
        dt <- start_time + dist()
      }
    } else {
      dt <- dist()
    }
    counter <<- counter + dt
    if (counter < stop_time) {
      return(dt)
    } else {
      return(-1)
    }
  }
}
