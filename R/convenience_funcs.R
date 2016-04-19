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
#' env <- simmer(verbose=TRUE) %>%
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
#' env <- simmer(verbose=TRUE) %>%
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
#' env <- simmer(verbose=TRUE) %>%
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
#' env <- simmer(verbose=TRUE) %>%
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
#' env <- simmer(verbose=TRUE) %>%
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

#' @importFrom R6 R6Class
Schedule <- R6Class("Schedule",
  public = list(
    initialize = function(timetable, period, values) { 
      if (!is.numeric(c(timetable, period, values)))
        stop("all arguments must be numeric")
      if (is.unsorted(timetable) || !all(period >= timetable) ||
          (timetable[length(timetable)] == period + timetable[1]))
        stop("invalid timetable")
      if (length(timetable) != length(values))
        stop("vector lengths must agree")
      if (length(timetable) < 2)
        stop("at least two values required")
      if (!all(values >= 0))
        stop("invalid values")
      private$timetable <- timetable
      private$period <- period
      private$values <- replace(values, values==Inf, -1)
      private$n <- length(private$timetable)
      self
    },
    
    print = function() {
      # some pretty printing here
    },
    
    get_init = function() {
      if (private$timetable[1] == 0)
        private$values[1]
      else
        private$values[private$n]
    },
    
    get_schedule = function() {
      if (private$timetable[1] == 0) list(
        intervals = c(private$timetable[1], diff(private$timetable), 
                      private$timetable[1] + private$period - private$timetable[private$n]),
        values = c(private$values, private$values[1]))
      else list(
        intervals = c(private$timetable[1], diff(private$timetable), 
                      private$timetable[1] + private$period - private$timetable[private$n]),
        values = c(private$values, private$values[1]))
    }
  ),
  
  private = list(
    timetable = NA,
    period = NA,
    values = NA,
    n = NA
  )
)

#' Generate a scheduling object
#' 
#' Resource convenience function to generate a scheduling object from a timetable specification.
#'
#' @param timetable absolute points in time in which the desired value changes.
#' @param period period of repetition.
#' @param values one value for each point in time.
#' 
#' @return Returns a Schedule object.
#' @export
#' 
#' @examples
#' # Schedule 3 units from 8 to 16 h
#' #          2 units from 16 to 24 h
#' #          1 units from 24 to 8 h
#' my_schedule <- schedule(c(8, 16, 24), 24, c(3, 2, 1))
schedule <- function(timetable, period, values) Schedule$new(timetable, period, values)
