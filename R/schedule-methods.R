# Copyright (C) 2016-2022 Iñaki Ucar
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

#' Generate a Scheduling Object
#'
#' Resource convenience function to generate a scheduling object from a
#' timetable specification.
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
#'
#' # Composition of schedules
#' sch1 <- schedule(c(8, 16), c(3, 0), period=24)
#' sch2 <- schedule(c(16, 24), c(2, 1), period=24)
#' all.equal(sch1 + sch2, capacity_schedule)
#'
schedule <- function(timetable, values, period=Inf) {
  stopifnot(is.numeric(c(timetable, period, values)))
  stopifnot(!is.unsorted(timetable), all(period >= timetable),
            timetable[length(timetable)] != timetable[1] + period)
  stopifnot(length(timetable) == length(values))
  stopifnot(length(timetable) >= 2)
  stopifnot(all(values >= 0))

  x <- list(
    timetable = timetable,
    values = replace(values, values == Inf, -1),
    period = replace(period, period == Inf, -1),
    n = length(timetable)
  )

  x$schedule <- if (x$period < 0) list(
    init = if (x$timetable[1] == 0) x$values[1] else 0,
    intervals = c(x$timetable[1], diff(x$timetable)),
    values = x$values,
    period = x$period
  ) else list( # periodic
    init = if (x$timetable[1] == 0) x$values[1] else x$values[x$n],
    intervals = c(x$timetable[1], diff(x$timetable),
                  x$timetable[1] + x$period - x$timetable[x$n]),
    values = c(x$values, x$values[1]),
    period = x$period
  )

  class(x) <- "schedule"
  x
}

#' @export
print.schedule <- function(x, ...) {
  out <- format(c(x$timetable, x$values), justify="right")
  timetable <- seq_along(x$timetable)
  cat(paste0(
    "schedule: period: ",
    ifelse(x$period > 0, x$period, Inf), "\n",
    "{ timetable: ", paste(out[timetable], collapse=" "), " }\n",
    "{ values:    ", paste(out[-timetable], collapse=" "), " }\n"
  ))
  invisible(x)
}

#' @export
`+.schedule` <- function(e1, e2) {
  if (!inherits(e1, "schedule") || !inherits(e2, "schedule"))
    stop("both operands must be 'schedule'")

  e1$values <- with(e1, replace(values, values == -1, Inf))
  e1$period <- with(e1, replace(period, period == -1, Inf))
  e2$values <- with(e2, replace(values, values == -1, Inf))
  e2$period <- with(e2, replace(period, period == -1, Inf))

  if (sum(is.finite(c(e1$period, e2$period))) == 1)
    stop("not compatible periods")

  if (is.finite(period <- lcm(e1$period, e2$period))) {
    e1$timetable <- e1$timetable +
      rep(seq(0, period-e1$period, e1$period), each=e1$n)
    e2$timetable <- e2$timetable +
      rep(seq(0, period-e2$period, e2$period), each=e2$n)
    e1$values <- rep(e1$values, period/e1$period)
    e2$values <- rep(e2$values, period/e2$period)
  }

  e1f <- stats::stepfun(e1$timetable, c(0, e1$values))
  e2f <- stats::stepfun(e2$timetable, c(0, e2$values))

  timetable <- unique(sort(c(e1$timetable, e2$timetable)))
  values <- e1f(timetable) + e2f(timetable)
  schedule(timetable, values, period)
}
