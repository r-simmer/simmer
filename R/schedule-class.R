# Copyright (C) 2014-2018 Iñaki Ucar and Bart Smeets
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

Schedule <- R6Class("schedule",
  public = list(
    initialize = function(timetable, values, period=Inf) {
      stopifnot(is.numeric(c(timetable, period, values)))
      stopifnot(!is.unsorted(timetable), all(period >= timetable),
                timetable[length(timetable)] != timetable[1] + period)
      stopifnot(length(timetable) == length(values))
      stopifnot(length(timetable) >= 2)
      stopifnot(all(values >= 0))

      private$timetable <- timetable
      private$values <- replace(values, values == Inf, -1)
      private$period <- replace(period, period == Inf, -1)
      private$n <- length(private$timetable)

      if (private$period < 0) private$compose_non_periodic()
      else private$compose_periodic()
      self
    },

    print = function() {
      cat(paste0(
        "schedule\n",
        "{ timetable: ", paste(private$timetable, collapse = " "),
        " | period: ", ifelse(private$period > 0, private$period, Inf), " }\n",
        "{ values: ", paste(private$values, collapse = " "), " }\n"
      ))
      invisible(self)
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

#' Generate a Scheduling Object
#'
#' Resource convenience function to generate a scheduling object from a timetable specification.
#'
#' @param timetable absolute points in time in which the desired value changes.
#' @param values one value for each point in time.
#' @param period period of repetition.
#'
#' @return Returns a \code{schedule} object.
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
schedule <- function(timetable, values, period=Inf) Schedule$new(timetable, values, period)
