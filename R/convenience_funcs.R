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
#' t0 <- create_trajectory() %>% timeout(0)
#' env <- simmer(verbose=TRUE) %>%
#'   add_generator("dummy", t0, at(0, c(1,10,30), 40, 43)) %>%
#'   run(100)
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
#' @inheritParams from
#'
#' @return Returns a generator function.
#' @seealso \code{\link{add_generator}}.
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
#' 
#' @return Returns a generator function.
#' @seealso \code{\link{add_generator}}.
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
        dt <- dist()
        dt[1] <- dt[1] + start_time
      }
    } else {
      dt <- dist()
    }
    len <- length(dt)
    dt <- dt[cumsum(dt) + counter < stop_time]
    counter <<- counter + sum(dt)
    if (len == length(dt)) return(dt)
    return(c(dt, -1))
  }
}

#' @importFrom R6 R6Class
simmer.schedule <- R6Class("simmer.schedule",
  public = list(
    initialize = function(timetable, values, period=Inf) {
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
      private$values <- replace(values, values == Inf, -1)
      private$period <- replace(period, period == Inf, -1)
      private$n <- length(private$timetable)
      
      if (private$period < 0) private$compose_non_periodic()
      else private$compose_periodic()
      self
    },
    
    print = function() {
      cat(paste0("simmer schedule\n",
                 "{ timetable: ", paste(private$timetable, collapse = " "),
                 " | period: ", ifelse(private$period > 0, private$period, Inf), " }\n",
                 "{ values: ", paste(private$values, collapse = " "), " }\n"))
    },
    
    get_schedule = function() { private$schedule }
  ),
  
  private = list(
    timetable = NA,
    period = NA,
    values = NA,
    n = NA,
    schedule = NA,
    
    compose_periodic = function() {
      intervals <- c(private$timetable[1], diff(private$timetable),
                     private$timetable[1] + private$period - private$timetable[private$n])
      values <- c(private$values, private$values[1])
      if (private$timetable[1] == 0) init <- private$values[1]
      else init <- private$values[private$n]
      private$schedule <- list(
        init = init,
        intervals = intervals,
        values = values,
        period = private$period)
    },
    
    compose_non_periodic = function() {
      private$schedule <- list(
        init = 0,
        intervals = c(private$timetable[1], diff(private$timetable)),
        values = private$values,
        period = private$period)
    }
  )
)

#' Generate a scheduling object
#' 
#' Resource convenience function to generate a scheduling object from a timetable specification.
#'
#' @param timetable absolute points in time in which the desired value changes.
#' @param values one value for each point in time.
#' @param period period of repetition.
#' 
#' @return Returns a Schedule object.
#' @seealso \code{\link{add_resource}}.
#' @export
#' 
#' @examples
#' # Schedule 3 units from 8 to 16 h
#' #          2 units from 16 to 24 h
#' #          1 units from 24 to 8 h
#' capacity_schedule <- schedule(c(8, 16, 24), c(3, 2, 1), period=24)
#' 
#' env <- simmer() %>%
#'   add_resource("dummy", capacity_schedule)
schedule <- function(timetable, values, period=Inf) simmer.schedule$new(timetable, values, period)
