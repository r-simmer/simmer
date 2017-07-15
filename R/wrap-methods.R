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
now.wrap <- function(.env) now.simmer(.env)

#' @export
peek.wrap <- function(.env, steps=1, verbose=FALSE) peek.simmer(.env, steps, verbose)

#' @export
get_mon_arrivals.wrap <- function(.envs, per_resource=FALSE, ongoing=FALSE)
  get_mon_arrivals.simmer(.envs, per_resource, ongoing)

#' @export
get_mon_attributes.wrap <- function(.envs) get_mon_attributes.simmer(.envs)

#' @export
get_mon_resources.wrap <- function(.envs, data=c("counts", "limits"))
  get_mon_resources.simmer(.envs, data)

#' @export
get_n_generated.wrap <- function(.env, generator) get_n_generated.simmer(.env, generator)

#' @export
get_capacity.wrap <- function(.env, resource) get_capacity.simmer(.env, resource)

#' @export
get_queue_size.wrap <- function(.env, resource) get_queue_size.simmer(.env, resource)

#' @export
get_server_count.wrap <- function(.env, resource) get_server_count.simmer(.env, resource)

#' @export
get_queue_count.wrap <- function(.env, resource) get_queue_count.simmer(.env, resource)
