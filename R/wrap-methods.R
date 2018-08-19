# Copyright (C) 2016-2018 Iñaki Ucar
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

#' Wrap a Simulation Environment
#'
#' This function extracts the monitored data from a simulation environment
#' making it accessible through the same methods. Only useful if you want
#' to parallelize heavy replicas (see the example below), because the C++
#' simulation backend is destroyed when the threads exit.
#'
#' @inheritParams reset
#'
#' @return Returns a simulation wrapper.
#' @seealso Methods for dealing with a simulation wrapper:
#' \code{\link{get_mon_arrivals}}, \code{\link{get_mon_attributes}}, \code{\link{get_mon_resources}},
#' \code{\link{get_n_generated}}, \code{\link{get_capacity}}, \code{\link{get_queue_size}},
#' \code{\link{get_server_count}}, \code{\link{get_queue_count}}.
#' @export
#'
#' @examples
#' \dontrun{
#' library(parallel)
#'
#' mm1 <- trajectory() %>%
#'   seize("server", 1) %>%
#'   timeout(function() rexp(1, 2)) %>%
#'   release("server", 1)
#'
#' envs <- mclapply(1:4, function(i) {
#'   simmer("M/M/1 example") %>%
#'     add_resource("server", 1) %>%
#'     add_generator("customer", mm1, function() rexp(1, 1)) %>%
#'     run(100) %>%
#'     wrap()
#' })
#' }
wrap <- function(.env) Wrap$new(.env)

#' @export
now.wrap <- now.simmer

#' @export
peek.wrap <- peek.simmer

#' @export
get_mon_arrivals.wrap <- get_mon_arrivals.simmer

#' @export
get_mon_attributes.wrap <- get_mon_attributes.simmer

#' @export
get_mon_resources.wrap <- get_mon_resources.simmer

#' @export
get_sources.wrap <- get_sources.simmer

#' @export
get_resources.wrap <- get_resources.simmer

#' @export
get_n_generated.wrap <- get_n_generated.simmer

#' @export
get_capacity.wrap <- get_capacity.simmer

#' @export
get_queue_size.wrap <- get_queue_size.simmer

#' @export
get_server_count.wrap <- get_server_count.simmer

#' @export
get_queue_count.wrap <- get_queue_count.simmer
