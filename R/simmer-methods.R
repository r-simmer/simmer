# Copyright (C) 2014-2015 Bart Smeets
# Copyright (C) 2015-2016 Bart Smeets and Iñaki Ucar
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

#' Create a Simulator
#'
#' This method initialises a simulation environment.
#'
#' @param name the name of the simulator.
#' @param verbose enable showing activity information.
#' @param mon monitor (in memory by default); see \code{\link{monitor}} for
#' other options.
#' @param log_level debugging level (see \code{\link{log_}}).
#'
#' @return Returns a simulation environment.
#' @seealso
#' Available methods by category:
#' \itemize{
#'
#' \item Simulation control: \code{\link{stepn}}, \code{\link{run}},
#' \code{\link{now}}, \code{\link{peek}}, \code{\link{reset}}
#'
#' \item Resources: \code{\link{add_resource}}, \code{\link{get_resources}},
#' \code{\link{get_capacity}}, \code{\link{get_queue_size}},
#' \code{\link{get_server_count}}, \code{\link{get_queue_count}},
#' \code{\link{get_capacity_selected}}, \code{\link{get_queue_size_selected}},
#' \code{\link{get_server_count_selected}}, \code{\link{get_queue_count_selected}}
#'
#' \item Sources: \code{\link{add_generator}}, \code{\link{add_dataframe}},
#' \code{\link{get_sources}}, \code{\link{get_n_generated}},
#' \code{\link{get_trajectory}}
#'
#' \item Data retrieval: \code{\link{get_mon_arrivals}},
#' \code{\link{get_mon_attributes}}, \code{\link{get_mon_resources}}
#'
#' }
#'
#' @export
#'
#' @examples
#' ## a simple trajectory that prints a message
#' t0 <- trajectory("my trajectory") %>%
#'   log_("arrival generated")
#'
#' ## create an empty simulation environment
#' env <- simmer("SuperDuperSim")
#' env
#'
#' ## add a generator and attach it to the trajectory above
#' env %>% add_generator("dummy", t0, function() 1)
#'
#' ## run for some time
#' env %>% run(until=4.5)
#' env %>% now()           # current simulation time
#' env %>% peek()          # time for the next event
#' env %>% stepn()         # execute next event
#'
simmer <- function(name="anonymous", verbose=FALSE, mon=monitor_mem(), log_level=0)
  Simmer$new(name, verbose, mon, log_level)

#' Reset a Simulator
#'
#' Reset the following components of a simulation environment:
#' time, event queue, resources, sources and statistics.
#'
#' @param .env the simulation environment.
#'
#' @return Returns the simulation environment.
#' @seealso \code{\link{stepn}}, \code{\link{run}}.
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
run <- function(.env, until=Inf, progress=NULL, steps=10) UseMethod("run")

#' @export
run.simmer <- function(.env, until=Inf, progress=NULL, steps=10) {
  check_args(until="number", progress=c("function", "NULL"), steps="number")
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
#' @param n number of events to simulate.
#' @export
stepn <- function(.env, n=1) UseMethod("stepn")

#' @export
stepn.simmer <- function(.env, n=1) .env$stepn(n)

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
#' Attach a new source of arrivals to a trajectory from a generator function.
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
#'
#' Other sources: \code{\link{add_dataframe}}.
#' @export
add_generator <- function(.env, name_prefix, trajectory, distribution, mon=1,
                          priority=0, preemptible=priority, restart=FALSE)
  UseMethod("add_generator")

#' @export
add_generator.simmer <- function(.env, name_prefix, trajectory, distribution, mon=1,
                                 priority=0, preemptible=priority, restart=FALSE)
  .env$add_generator(name_prefix, trajectory, distribution, mon, priority, preemptible, restart)

#' Add a Data Frame
#'
#' Attach a new source of arrivals to a trajectory from a data frame.
#'
#' @inheritParams add_generator
#' @param data a data frame with, at least, a column of (inter)arrival times (see details).
#' @param batch number of arrivals generated at a time. Arrivals are read from
#' the data frame and attached to the trajectory in batches depending on this
#' value. In general, it should not be changed.
#' @param col_time name of the time column in the data frame.
#' @param time type of time column: \emph{interarrival}, if the time column
#' contains interarrival times, or \emph{absolute}, if the time column contains
#' absolute arrival times.
#' @param col_attributes vector of names of the attributes columns (see details).
#' @param col_priority name of the priority column.
#' @param col_preemptible name of the preemptible column.
#' @param col_restart name of the restart column.
#'
#' @return Returns the simulation environment.
#'
#' @details The data frame provided must have, at least, a column of (inter)arrival
#' times. This method will look for it under the name \code{"time"} by default,
#' although this can be changed with the \code{col_time} parameter.
#'
#' If there is any column named \code{col_priority="priority"},
#' \code{col_preemptible=priority} or \code{col_restart="restart"}, they will be
#' used to set the prioritization values for each arrival (see \code{\link{add_generator}}).
#'
#' If there are additional columns (with \code{col_attributes=NULL}, by default),
#' they will be assigned to arrival attributes named after each column name. All
#' these columns must be numeric (or logical). Otherwise, if a vector of column
#' names is specified, only these will be assigned as attributes and the rest of
#' the columns will be ignored.
#'
#' A value of \code{batch=Inf} means that the whole data frame will be attached
#' at the beginning of the simulation. This is not desirable in general, because
#' the performance of the event queue is degraded when it is populated with too
#' many events. On the other hand, a low value results in an increased overhead
#' due to many function calls. The default value has been tested to provide a
#' good trade-off.
#'
#' @seealso Other sources: \code{\link{add_generator}}.
#' @export
add_dataframe <- function(.env, name_prefix, trajectory, data, mon=1, batch=50,
                          col_time="time", time=c("interarrival", "absolute"),
                          col_attributes=NULL, col_priority="priority",
                          col_preemptible=col_priority, col_restart="restart")
  UseMethod("add_dataframe")

#' @export
add_dataframe.simmer <- function(.env, name_prefix, trajectory, data, mon=1, batch=50,
                                 col_time="time", time=c("interarrival", "absolute"),
                                 col_attributes=NULL, col_priority="priority",
                                 col_preemptible=col_priority, col_restart="restart")
  .env$add_dataframe(name_prefix, trajectory, data, mon, batch, col_time, time,
                     col_attributes, col_priority, col_preemptible, col_restart)

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
get_mon_attributes <- function(.envs)
  UseMethod("get_mon_attributes", unlist(list(.envs))[[1]])

#' @export
get_mon_attributes.simmer <- function(.envs) envs_apply(.envs, "get_mon_attributes")

#' @rdname get_mon
#' @export
get_mon_resources <- function(.envs)
  UseMethod("get_mon_resources", unlist(list(.envs))[[1]])

#' @export
get_mon_resources.simmer <- function(.envs)
  envs_apply(.envs, "get_mon_resources")

#' Get Sources and Resources Defined
#'
#' Get a list of names of sources or resources defined in a simulation environment.
#'
#' @inheritParams reset
#'
#' @return A character vector.
#' @export
get_sources <- function(.env) UseMethod("get_sources")

#' @export
get_sources.simmer <- function(.env) names(.env$get_sources())

#' @rdname get_sources
#' @export
get_resources <- function(.env) UseMethod("get_resources")

#' @export
get_resources.simmer <- function(.env) names(.env$get_resources())

#' Get Process Parameters
#'
#' Getters for processes (sources and arrivals) number of arrivals generated
#' by a source, the name of the active arrival, an attribute from the active
#' arrival or a global one, and prioritization values.
#'
#' @inheritParams reset
#' @param source the name of the source.
#'
#' @details \code{get_n_generated} returns the number of arrivals generated by a
#' given source. \code{get_trajectory} returns the trajectory to which it is
#' attached.
#'
#' \code{get_name} returns the number of the running arrival. \code{get_attribute}
#' returns a running arrival's attributes. If a provided key was not previously
#' set, it returns a missing value. \code{get_global} returns a global attribute.
#' \code{get_prioritization} returns a running arrival's prioritization values.
#' \code{get_name}, \code{get_attribute} and \code{get_prioritization} are meant
#' to be used inside a trajectory; otherwise, there will be no arrival running
#' and these functions will throw an error.
#'
#' @seealso \code{\link{get_sources}}, \code{\link{set_trajectory}},
#' \code{\link{set_attribute}}, \code{\link{set_global}},
#' \code{\link{set_prioritization}}.
#' @export
get_n_generated <- function(.env, source) UseMethod("get_n_generated")

#' @export
get_n_generated.simmer <- function(.env, source) .env$get_n_generated(source)

#' @rdname get_n_generated
#' @export
get_trajectory <- function(.env, source) UseMethod("get_trajectory")

#' @export
get_trajectory.simmer <- function(.env, source) .env$get_trajectory(source)

#' @rdname get_n_generated
#' @export
get_name <- function(.env) UseMethod("get_name")

#' @export
get_name.simmer <- function(.env) .env$get_name()

#' @param keys the attribute name(s).
#' @inheritParams set_attribute
#'
#' @rdname get_n_generated
#' @export
get_attribute <- function(.env, keys, global=FALSE) {
  if (global) {
    .Deprecated("get_global", old="get_attribute(global=TRUE)")
    get_global(.env, keys)
  }
  else UseMethod("get_attribute")
}

#' @export
get_attribute.simmer <- function(.env, keys, global=FALSE) .env$get_attribute(keys)

#' @rdname get_n_generated
#' @export
get_global <- function(.env, keys) UseMethod("get_global")

#' @export
get_global.simmer <- function(.env, keys) .env$get_global(keys)

#' @rdname get_n_generated
#' @export
get_prioritization <- function(.env) UseMethod("get_prioritization")

#' @export
get_prioritization.simmer <- function(.env) .env$get_prioritization()

#' Get Resource Parameters
#'
#' Getters for resources: server capacity/count and queue size/count.
#'
#' @inheritParams reset
#' @inheritParams select
#' @param resource the name of the resource.
#'
#' @return Return a numeric value.
#' @seealso \code{\link{get_resources}}, \code{\link{set_capacity}},
#' \code{\link{set_queue_size}}.
#' @export
get_capacity <- function(.env, resource) UseMethod("get_capacity")

#' @export
get_capacity.simmer <- function(.env, resource) .env$get_capacity(resource)

#' @rdname get_capacity
#' @export
get_capacity_selected <- function(.env, id=0) UseMethod("get_capacity_selected")

#' @export
get_capacity_selected.simmer <- function(.env, id=0) .env$get_capacity(NA, id)

#' @rdname get_capacity
#' @export
get_queue_size <- function(.env, resource) UseMethod("get_queue_size")

#' @export
get_queue_size.simmer <- function(.env, resource) .env$get_queue_size(resource)

#' @rdname get_capacity
#' @export
get_queue_size_selected <- function(.env, id=0) UseMethod("get_queue_size_selected")

#' @export
get_queue_size_selected.simmer <- function(.env, id=0) .env$get_queue_size(NA, id)

#' @rdname get_capacity
#' @export
get_server_count <- function(.env, resource) UseMethod("get_server_count")

#' @export
get_server_count.simmer <- function(.env, resource) .env$get_server_count(resource)

#' @rdname get_capacity
#' @export
get_server_count_selected <- function(.env, id=0) UseMethod("get_server_count_selected")

#' @export
get_server_count_selected.simmer <- function(.env, id=0) .env$get_server_count(NA, id)

#' @rdname get_capacity
#' @export
get_queue_count <- function(.env, resource) UseMethod("get_queue_count")

#' @export
get_queue_count.simmer <- function(.env, resource) .env$get_queue_count(resource)

#' @rdname get_capacity
#' @export
get_queue_count_selected <- function(.env, id=0) UseMethod("get_queue_count_selected")

#' @export
get_queue_count_selected.simmer <- function(.env, id=0) .env$get_queue_count(NA, id)
