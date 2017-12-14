#' Create a Simulator
#'
#' This method initialises a simulation environment.
#'
#' @param name the name of the simulator.
#' @param verbose enable showing activity information.
#'
#' @return Returns a simulation environment.
#' @seealso Methods for dealing with a simulation environment:
#' \code{\link{reset}}, \code{\link{now}}, \code{\link{peek}}, \code{\link{onestep}}, \code{\link{run}},
#' \code{\link{add_resource}}, \code{\link{add_generator}}, \code{\link{get_mon_arrivals}},
#' \code{\link{get_mon_attributes}}, \code{\link{get_mon_resources}}, \code{\link{get_n_generated}},
#' \code{\link{get_capacity}}, \code{\link{get_queue_size}},
#' \code{\link{get_server_count}}, \code{\link{get_queue_count}}.
#' @export
#'
#' @examples
#' t0 <- trajectory("my trajectory") %>%
#'   ## add an intake activity
#'   seize("nurse", 1) %>%
#'   timeout(function() rnorm(1, 15)) %>%
#'   release("nurse", 1) %>%
#'   ## add a consultation activity
#'   seize("doctor", 1) %>%
#'   timeout(function() rnorm(1, 20)) %>%
#'   release("doctor", 1) %>%
#'   ## add a planning activity
#'   seize("administration", 1) %>%
#'   timeout(function() rnorm(1, 5)) %>%
#'   release("administration", 1)
#'
#' env <- simmer("SuperDuperSim") %>%
#'   add_resource("nurse", 1) %>%
#'   add_resource("doctor", 2) %>%
#'   add_resource("administration", 1) %>%
#'   add_generator("patient", t0, function() rnorm(1, 10, 2)) %>%
#'   run(until=80)
#'
simmer <- function(name="anonymous", verbose=FALSE) Simmer$new(name, verbose)

#' Reset a Simulator
#'
#' Reset the following components of a simulation environment:
#' time, event queue, resources, generators and statistics.
#'
#' @param .env the simulation environment.
#'
#' @return Returns the simulation environment.
#' @seealso \code{\link{onestep}}, \code{\link{run}}.
#' @export
reset <- function(.env) UseMethod("reset")

#' @export
reset.simmer <- function(.env) .env$reset()

#' Run a Simulation
#'
#' Execute steps until a given criterion.
#'
#' @inheritParams reset
#' @param until stop time.
#' @param progress optional callback to show the progress of the simulation. The
#' completed ratio is periodically passed as argument to the callback.
#' @param steps number of steps to show as progress (it takes effect only if
#' \code{progress} is provided).
#'
#' @return Returns the simulation environment.
#' @seealso \code{\link{reset}}.
#' @export
run <- function(.env, until=1000, progress=NULL, steps=10) UseMethod("run")

#' @export
run.simmer <- function(.env, until=1000, progress=NULL, steps=10) {
  check_args(until, progress, steps, types=c("number", "function or NULL", "number"))
  if (is.function(progress)) {
    progress(0)
    for (i in seq(until/steps, until, until/steps)) {
      .env$run(until=i)
      progress(i/until)
    }
    .env
  } else .env$run(until=until)
}

#' @rdname run
#' @export
onestep <- function(.env) UseMethod("onestep")

#' @export
onestep.simmer <- function(.env) .env$step()

#' Simulation Time
#'
#' Get the current simulation time.
#'
#' @inheritParams reset
#'
#' @return Returns a numeric value.
#' @seealso \code{\link{peek}}.
#' @export
now <- function(.env) UseMethod("now")

#' @export
now.simmer <- function(.env) .env$now()

#' Peek Next Events
#'
#' Look for future events in the event queue and (optionally) obtain info about them.
#'
#' @inheritParams reset
#' @param steps number of steps to peek.
#' @param verbose show additional information (i.e., the name of the process) about future events.
#'
#' @return Returns numeric values if \code{verbose=F} and a data frame otherwise.
#' @seealso \code{\link{now}}.
#' @export
peek <- function(.env, steps=1, verbose=FALSE) UseMethod("peek")

#' @export
peek.simmer <- function(.env, steps=1, verbose=FALSE) .env$peek(steps, verbose)

#' Add a Resource
#'
#' Define a new resource in a simulation environment.
#'
#' @inheritParams reset
#' @param name the name of the resource.
#' @param capacity the capacity of the server.
#' @param queue_size the size of the queue.
#' @param mon whether the simulator must monitor this resource or not.
#' @param preemptive whether arrivals in the server can be preempted or not based
#' on seize priorities.
#' @param preempt_order if the resource is preemptive and preemption occurs with
#' more than one arrival in the server, this parameter defines which arrival should
#' be preempted first. It must be \code{fifo} (First In First Out: older preemptible
#' tasks are preempted first) or \code{lifo} (Last In First Out: newer preemptible tasks
#' are preempted first).
#' @param queue_size_strict if the resource is preemptive and preemption occurs,
#' this parameter controls whether the \code{queue_size} is a hard limit. By default,
#' preempted arrivals go to a dedicated queue, so that \code{queue_size} may be
#' exceeded. If this option is \code{TRUE}, preempted arrivals go to the standard
#' queue, and the maximum \code{queue_size} is guaranteed (rejection may occur).
#'
#' @return Returns the simulation environment.
#' @seealso Convenience functions: \code{\link{schedule}}.
#' @export
add_resource <- function(.env, name, capacity=1, queue_size=Inf, mon=TRUE, preemptive=FALSE,
                         preempt_order=c("fifo", "lifo"), queue_size_strict=FALSE)
  UseMethod("add_resource")

#' @export
add_resource.simmer <- function(.env, name, capacity=1, queue_size=Inf, mon=TRUE, preemptive=FALSE,
                                preempt_order=c("fifo", "lifo"), queue_size_strict=FALSE)
  .env$add_resource(name, capacity, queue_size, mon, preemptive, preempt_order, queue_size_strict)

#' Add a Generator
#'
#' Define a new generator of arrivals in a simulation environment.
#'
#' @inheritParams reset
#' @param name_prefix the name prefix of the generated arrivals.
#' @param trajectory the trajectory that the generated arrivals will follow (see
#' \code{\link{trajectory}}).
#' @param distribution a function modelling the interarrival times (returning a
#' negative value stops the generator).
#' @param mon whether the simulator must monitor the generated arrivals or not
#' (0 = no monitoring, 1 = simple arrival monitoring, 2 = level 1 + arrival
#' attribute monitoring)
#' @param priority the priority of each arrival (a higher integer equals higher
#' priority; defaults to the minimum priority, which is 0).
#' @param preemptible if a seize occurs in a preemptive resource, this parameter
#' establishes the minimum incoming priority that can preempt these arrivals (an
#' arrival with a priority greater than \code{preemptible} gains the resource). In
#' any case, \code{preemptible} must be equal or greater than \code{priority}, and
#' thus only higher priority arrivals can trigger preemption.
#' @param restart whether the activity must be restarted after being preempted.
#'
#' @return Returns the simulation environment.
#' @seealso Convenience functions: \code{\link{at}}, \code{\link{from}},
#' \code{\link{to}}, \code{\link{from_to}}.
#' @export
add_generator <- function(.env, name_prefix, trajectory, distribution, mon=1,
                          priority=0, preemptible=priority, restart=FALSE)
  UseMethod("add_generator")

#' @export
add_generator.simmer <- function(.env, name_prefix, trajectory, distribution, mon=1,
                                 priority=0, preemptible=priority, restart=FALSE)
  .env$add_generator(name_prefix, trajectory, distribution, mon, priority, preemptible, restart)

#' Monitoring Statistics
#'
#' Getters for obtaining monitored data (if any) about arrivals, attributes and resources.
#'
#' @param .envs the simulation environment (or a list of environments).
#' @param per_resource if \code{TRUE}, statistics will be reported on a per-resource basis.
#' @param ongoing if \code{TRUE}, ongoing arrivals will be reported. The columns
#' \code{end_time} and \code{finished} of these arrivals are reported as \code{NA}s.
#'
#' @return Returns a data frame.
#' @name get_mon
#' @export
get_mon_arrivals <- function(.envs, per_resource=FALSE, ongoing=FALSE)
  UseMethod("get_mon_arrivals", unlist(list(.envs))[[1]])

#' @export
get_mon_arrivals.simmer <- function(.envs, per_resource=FALSE, ongoing=FALSE)
  envs_apply(.envs, "get_mon_arrivals", per_resource, ongoing)

#' @rdname get_mon
#' @export
get_mon_attributes <- function(.envs) UseMethod("get_mon_attributes", unlist(list(.envs))[[1]])

#' @export
get_mon_attributes.simmer <- function(.envs) envs_apply(.envs, "get_mon_attributes")

#' @param data whether to retrieve the "counts", the "limits" or both.
#' @rdname get_mon
#' @export
get_mon_resources <- function(.envs, data=c("counts", "limits"))
  UseMethod("get_mon_resources", unlist(list(.envs))[[1]])

#' @export
get_mon_resources.simmer <- function(.envs, data=c("counts", "limits"))
  envs_apply(.envs, "get_mon_resources", data)

#' Get Process Parameters
#'
#' Getters for processes (generators and arrivals) number of arrivals generated
#' by a generator, the name of the active arrival, an attribute from the active
#' arrival or a global one, and prioritization values.
#'
#' @inheritParams reset
#' @param generator the name of the generator.
#'
#' @details \code{get_n_generated} returns the number of arrivals generated by a
#' given generator.
#'
#' \code{get_name} returns the number of the running arrival. \code{get_attribute}
#' returns a running arrival's attributes or global ones. If a provided key was
#' not previously set, it returns a missing value. \code{get_global} is a shortcut
#' for \code{get_attribute(global=TRUE)}. \code{get_prioritization} returns a
#' running arrival's prioritization values. \code{get_name}, \code{get_attribute}
#' and \code{get_prioritization} are meant to be used inside a trajectory; otherwise,
#' there will be no arrival running and these functions will throw an error.
#'
#' @seealso \code{\link{set_attribute}}, \code{\link{set_prioritization}}.
#' @export
get_n_generated <- function(.env, generator) UseMethod("get_n_generated")

#' @export
get_n_generated.simmer <- function(.env, generator) .env$get_n_generated(generator)

#' @rdname get_n_generated
#' @export
get_name <- function(.env) UseMethod("get_name")

#' @export
get_name.simmer <- function(.env) .env$get_name()

#' @param keys the attribute name(s).
#' @param global if \code{TRUE}, the attribute will be global instead of per-arrival.
#'
#' @rdname get_n_generated
#' @export
get_attribute <- function(.env, keys, global=FALSE) UseMethod("get_attribute")

#' @export
get_attribute.simmer <- function(.env, keys, global=FALSE) .env$get_attribute(keys, global)

#' @rdname get_n_generated
#' @export
get_global <- function(.env, keys) get_attribute(.env, keys, TRUE)

#' @rdname get_n_generated
#' @export
get_prioritization <- function(.env) UseMethod("get_prioritization")

#' @export
get_prioritization.simmer <- function(.env) .env$get_prioritization()

#' Get Resource Parameters
#'
#' Getters for resources: server capacity/count/preemptiveness and queue size/count.
#'
#' @inheritParams reset
#' @param resource the name of the resource.
#'
#' @return Return a numeric value. \code{is_preemptive} returns a boolean.
#' @seealso \code{\link{set_capacity}}, \code{\link{set_queue_size}}.
#' @export
get_capacity <- function(.env, resource) UseMethod("get_capacity")

#' @export
get_capacity.simmer <- function(.env, resource) .env$get_capacity(resource)

#' @rdname get_capacity
#' @export
get_queue_size <- function(.env, resource) UseMethod("get_queue_size")

#' @export
get_queue_size.simmer <- function(.env, resource) .env$get_queue_size(resource)

#' @rdname get_capacity
#' @export
get_server_count <- function(.env, resource) UseMethod("get_server_count")

#' @export
get_server_count.simmer <- function(.env, resource) .env$get_server_count(resource)

#' @rdname get_capacity
#' @export
get_queue_count <- function(.env, resource) UseMethod("get_queue_count")

#' @export
get_queue_count.simmer <- function(.env, resource) .env$get_queue_count(resource)

#' @rdname get_capacity
#' @export
is_preemptive <- function(.env, resource) UseMethod("is_preemptive")

#' @export
is_preemptive.simmer <- function(.env, resource) .env$is_preemptive(resource)
