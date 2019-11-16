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
#' \code{\link{get_mon_arrivals}}, \code{\link{get_mon_attributes}},
#' \code{\link{get_mon_resources}}, \code{\link{get_n_generated}},
#' \code{\link{get_capacity}}, \code{\link{get_queue_size}},
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
wrap <- function(.env) {
  check_args(.env="simmer")

  env <- list2env(list(
    name = .env$name,
    now_val = now(.env),
    peek_val = peek(.env, Inf, TRUE),
    resources = .env$resources,
    sources = .env$sources,
    globals = .env$globals,
    mon_arrivals = get_mon_arrivals(.env, ongoing = TRUE),
    mon_arrivals_res = get_mon_arrivals(.env, TRUE, ongoing = TRUE),
    mon_attributes = get_mon_attributes(.env),
    mon_resources = get_mon_resources(.env),
    n_generated = list(),
    capacity = list(),
    queue_size = list(),
    server_count = list(),
    queue_count = list()
  ))

  sources <- names(env$sources)
  resources <- names(env$resources)
  if (!is.null(sources))
    env$n_generated[sources] <- get_n_generated(.env, sources)
  if (!is.null(resources)) {
    env$capacity[resources] <- get_capacity(.env, resources)
    env$queue_size[resources] <- get_queue_size(.env, resources)
    env$server_count[resources] <- get_server_count(.env, resources)
    env$queue_count[resources] <- get_queue_count(.env, resources)
  }

  class(env) <- "wrap"
  env
}

#' @export
print.wrap <- print.simmer

#' @export
now.wrap <- function(.env) .env$now_val

#' @export
peek.wrap <- function(.env, steps=1, verbose=FALSE) {
  check_args(steps="numeric", verbose="flag")

  steps <- min(positive(steps), nrow(.env$peek_val))
  ret <- .env$peek_val[0:steps, ]
  if (!verbose) ret$time
  else ret # nocov
}

#' @export
get_mon_arrivals.wrap <- function(.envs, per_resource=FALSE, ongoing=FALSE) {
  envs_apply(.envs, function(x) {
    if (per_resource) {
      if (!ongoing)
        stats::na.omit(x$mon_arrivals_res)
      else x$mon_arrivals_res
    } else {
      if (!ongoing)
        stats::na.omit(x$mon_arrivals)
      else x$mon_arrivals
    }
  })
}

#' @export
get_mon_attributes.wrap <- function(.envs)
  envs_apply(.envs, function(x) x$mon_attributes)

#' @export
get_mon_resources.wrap <- function(.envs)
  envs_apply(.envs, function(x) x$mon_resources)

#' @export
get_sources.wrap <- get_sources.simmer

#' @export
get_resources.wrap <- get_resources.simmer

check.wrap <- function(.env, entities) {
  comp <- as.character(substitute(entities))
  found <- entities %in% names(.env[[comp]])
  if (any(!found))
    stop(comp, " '", paste(entities[!found], collapse=", "), "' not found")
  entities
}

#' @export
get_n_generated.wrap <- function(.env, sources)
  unlist(.env$n_generated[check.wrap(.env, sources)], use.names=FALSE)

#' @export
get_capacity.wrap <- function(.env, resources)
  unlist(.env$capacity[check.wrap(.env, resources)], use.names=FALSE)

#' @export
get_queue_size.wrap <- function(.env, resources)
  unlist(.env$queue_size[check.wrap(.env, resources)], use.names=FALSE)

#' @export
get_server_count.wrap <- function(.env, resources)
  unlist(.env$server_count[check.wrap(.env, resources)], use.names=FALSE)

#' @export
get_queue_count.wrap <- function(.env, resources)
  unlist(.env$queue_count[check.wrap(.env, resources)], use.names=FALSE)
