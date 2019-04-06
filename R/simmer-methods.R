# Copyright (C) 2014-2015 Bart Smeets
# Copyright (C) 2015-2016 Bart Smeets and Iñaki Ucar
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
#' \code{\link{get_server_count_selected}}, \code{\link{get_queue_count_selected}},
#' \code{\link{get_seized}}, \code{\link{get_seized_selected}},
#' \code{\link{get_selected}}
#'
#' \item Sources: \code{\link{add_generator}}, \code{\link{add_dataframe}},
#' \code{\link{get_sources}}, \code{\link{get_n_generated}},
#' \code{\link{get_trajectory}}
#'
#' \item Globals: \code{\link{add_global}}, \code{\link{get_global}}
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
simmer <- function(name="anonymous", verbose=FALSE, mon=monitor_mem(), log_level=0) {
  check_args(name="string", verbose="flag", mon="monitor", log_level="number")

  env <- list2env(list(
    name = name,
    mon = mon,
    resources = list(),
    sources = list(),
    globals = list(),
    sim_obj = Simulator__new(name, verbose, mon$xptr, log_level)
  ))

  class(env) <- "simmer"
  env
}

#' @export
print.simmer <- function(x, ...) {
  cat(paste0(
    "simmer environment: ", x$name,
    " | now: ", now(x), " | next: ", peek(x), "\n",
    "{ Monitor: ", x$mon$name, " }\n"
  ))
  for (name in names(x$mon$handlers)) cat(paste0(
    "  { ", name, ": ", x$mon$handlers[[name]], " }\n"
  ))
  for (name in names(x$resources)) cat(paste0(
    "{ Resource: ", name,
    " | monitored: ", x$resources[[name]][["mon"]],
    " | server status: ", get_server_count(x, name),
    "(", get_capacity(x, name), ")",
    " | queue status: ", get_queue_count(x, name),
    "(", get_queue_size(x, name), ") }\n"
  ))
  for (name in names(x$sources)) cat(paste0(
    "{ Source: ", name,
    " | monitored: ", x$sources[[name]][["mon"]],
    " | n_generated: ", get_n_generated(x, name), " }\n"
  ))
  for (name in names(x$globals)) {
    value <- x$globals[[name]]
    is_schedule <- inherits(value, "schedule")
    if (is_schedule) value <- value$schedule$init
    cat(paste0(
      "{ Global: ", name, " | schedule: ", is_schedule,
      " | initial value: ", value, " }\n"
    ))
  }
  invisible(x)
}

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
reset.simmer <- function(.env) {
  reset_(.env$sim_obj)
  .env
}

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
      run_(.env$sim_obj, i)
      progress(i/until)
    }
  } else run_(.env$sim_obj, until)
  .env
}

#' @rdname run
#' @param n number of events to simulate.
#' @export
stepn <- function(.env, n=1) UseMethod("stepn")

#' @export
stepn.simmer <- function(.env, n=1) {
  stepn_(.env$sim_obj, n)
  .env
}

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
now.simmer <- function(.env) now_(.env$sim_obj)

#' Peek Next Events
#'
#' Look for future events in the event queue and (optionally) obtain info about them.
#'
#' @inheritParams reset
#' @param steps number of steps to peek.
#' @param verbose show additional information (i.e., the name of the process)
#' about future events.
#'
#' @return Returns numeric values if \code{verbose=F} and a data frame otherwise.
#' @seealso \code{\link{now}}.
#' @export
peek <- function(.env, steps=1, verbose=FALSE) UseMethod("peek")

#' @export
peek.simmer <- function(.env, steps=1, verbose=FALSE) {
  check_args(steps="number", verbose="flag")
  ret <- peek_(.env$sim_obj, steps)
  if (!verbose) ret$time
  else ret # nocov
}

#' Add a Resource
#'
#' Define a new resource in a simulation environment.
#'
#' @inheritParams reset
#' @param name the name of the resource.
#' @param capacity the capacity of the server, either a numeric or a
#' \code{\link{schedule}}, so that the value may change during the simulation.
#' @param queue_size the size of the queue, either a numeric or a
#' \code{\link{schedule}}, so that the value may change during the simulation.
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
#' Whenever an arrival is rejected (due to a server drop or a queue drop), it
#' will set the \code{finished} flag to \code{FALSE} in the output of
#' \code{\link{get_mon_arrivals}}. Unfinished arrivals can be handled with a
#' drop-out trajectory that can be set using the \code{\link{handle_unfinished}}
#' activity.
#'
#' @return Returns the simulation environment.
#' @seealso Convenience functions: \code{\link{schedule}}.
#' @export
add_resource <- function(.env, name, capacity=1, queue_size=Inf, mon=TRUE,
                         preemptive=FALSE, preempt_order=c("fifo", "lifo"),
                         queue_size_strict=FALSE)
  UseMethod("add_resource")

#' @export
add_resource.simmer <- function(.env, name, capacity=1, queue_size=Inf, mon=TRUE,
                                preemptive=FALSE, preempt_order=c("fifo", "lifo"),
                                queue_size_strict=FALSE)
{
  check_args(
    name = "string",
    capacity = c("number", "schedule"),
    queue_size = c("number", "schedule"),
    mon = "flag",
    preemptive = "flag",
    queue_size_strict = "flag"
  )
  preempt_order <- match.arg(preempt_order)

  if (inherits(capacity, "schedule")) {
    capacity_schedule <- capacity
    capacity <- capacity_schedule$schedule$init
  } else capacity_schedule <- NA

  if (inherits(queue_size, "schedule")) {
    queue_size_schedule <- queue_size
    queue_size <- queue_size_schedule$schedule$init
  } else queue_size_schedule <- NA

  ret <- add_resource_(.env$sim_obj, name, capacity, queue_size, mon,
                       preemptive, preempt_order, queue_size_strict)
  if (ret) .env$resources[[name]] <- c(mon=mon, preemptive=preemptive)

  if (inherits(capacity_schedule, "schedule"))
    add_resource_manager_(.env$sim_obj, name, "capacity", capacity,
                          capacity_schedule$schedule$intervals,
                          capacity_schedule$schedule$values,
                          capacity_schedule$schedule$period)
  if (inherits(queue_size_schedule, "schedule"))
    add_resource_manager_(.env$sim_obj, name, "queue_size", queue_size,
                          queue_size_schedule$schedule$intervals,
                          queue_size_schedule$schedule$values,
                          queue_size_schedule$schedule$period)
  .env
}

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
{
  check_args(
    name_prefix = "string",
    trajectory = "trajectory",
    distribution = "function",
    mon = "flag",
    priority = "number",
    preemptible = "number",
    restart = "flag"
  )
  ret <- add_generator_(.env$sim_obj, name_prefix, trajectory[],
                        make_resetable(distribution), mon,
                        priority, preemptible, restart)
  if (ret) .env$sources[[name_prefix]] <- c(mon=mon)
  .env
}

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
{
  check_args(
    name_prefix = "string",
    trajectory = "trajectory",
    data = "data.frame",
    mon = "flag",
    batch = "number",
    col_time = "string",
    col_attributes = c("string vector", "NULL"),
    col_priority = c("string", "NULL"),
    col_preemptible = c("string", "NULL"),
    col_restart = c("string", "NULL")
  )
  time <- match.arg(time)

  col_attributes <- as.character(col_attributes)
  col_priority <- intersect(col_priority, names(data))
  col_preemptible <- intersect(col_preemptible, names(data))
  col_restart <- intersect(col_restart, names(data))
  col_names <- c(col_time, col_priority, col_preemptible, col_restart, col_attributes)
  col_undef <- setdiff(col_names, names(data))

  if (length(col_undef))
    stop(get_caller(), ": columns '", paste0(col_undef, collapse="', '"),
         "' are not defined in ", as.character(substitute(data)), call.=FALSE)

  if (!length(col_attributes)) {
    col_attributes <- setdiff(names(data), col_names)
    col_names <- c(col_names, col_attributes)
  }

  for (col_name in col_names) {
    if (!(is.numeric(data[[col_name]]) || is.logical(data[[col_name]])))
      stop(get_caller(), ": column '", col_name, "' is not numeric", call.=FALSE)
  }

  if (any(is.na(data[[col_time]])) || any(data[[col_time]] < 0))
    stop(get_caller(), ": time must be positive", call.=FALSE)

  if (time == "absolute") {
    if (is.unsorted(data[[col_time]]))
      stop(get_caller(), ": unsorted absolute time provided", call.=FALSE)
    data[[col_time]] <- c(data[[col_time]][1], diff(data[[col_time]]))
  }

  ret <- add_dataframe_(.env$sim_obj, name_prefix, trajectory[], data, mon, batch,
                        col_time, col_attributes, col_priority, col_preemptible, col_restart)
  if (ret) .env$sources[[name_prefix]] <- c(mon=mon)
  .env
}

#' Add a Global Attribute
#'
#' Attach a global variable to the simulation.
#'
#' @inheritParams reset
#' @param key the attribute name.
#' @param value the value to set, either a numeric or a \code{\link{schedule}},
#' so that the global may change during the simulation.
#'
#' @return Returns the simulation environment.
#' @seealso Convenience functions: \code{\link{schedule}}.
#' @export
add_global <- function(.env, key, value) UseMethod("add_global")

#' @export
add_global.simmer <- function(.env, key, value) {
  check_args(key = "string", value = c("numeric", "schedule"))

  intervals <- values <- numeric(0); period <- -1
  if (inherits(value, "schedule")) {
    intervals <- value$schedule$intervals
    values <- value$schedule$values
    period <- value$schedule$period
    value <- value$schedule$init
  }

  ret <- add_global_manager_(.env$sim_obj, key, value, intervals, values, period)

  if (ret) .env$globals[[key]] <- value
  .env
}

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
get_mon_arrivals.simmer <- function(.envs, per_resource=FALSE, ongoing=FALSE) {
  envs_apply(.envs, function() {
    if (ongoing) record_ongoing_(.envs$sim_obj, per_resource)
    .envs$mon$get_arrivals(per_resource)
  })
}

#' @rdname get_mon
#' @export
get_mon_attributes <- function(.envs)
  UseMethod("get_mon_attributes", unlist(list(.envs))[[1]])

#' @export
get_mon_attributes.simmer <- function(.envs) {
  envs_apply(.envs, function() .envs$mon$get_attributes())
}

#' @rdname get_mon
#' @export
get_mon_resources <- function(.envs)
  UseMethod("get_mon_resources", unlist(list(.envs))[[1]])

#' @export
get_mon_resources.simmer <- function(.envs) {
  envs_apply(.envs, function() {
    monitor_data <- .envs$mon$get_resources()
    monitor_data$capacity <-
      replace(monitor_data$capacity, monitor_data$capacity == -1, Inf)
    monitor_data$queue_size <-
      replace(monitor_data$queue_size, monitor_data$queue_size == -1, Inf)
    monitor_data$system <- monitor_data$server + monitor_data$queue
    monitor_data$limit <- monitor_data$capacity + monitor_data$queue_size
    monitor_data
  })
}

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
get_sources.simmer <- function(.env) names(.env$sources)

#' @rdname get_sources
#' @export
get_resources <- function(.env) UseMethod("get_resources")

#' @export
get_resources.simmer <- function(.env) names(.env$resources)

#' Get Process Parameters
#'
#' Getters for processes (sources and arrivals) number of arrivals generated
#' by a source, the name of the active arrival, an attribute from the active
#' arrival or a global one, and prioritization values.
#'
#' @inheritParams reset
#' @param sources one or more resource names.
#'
#' @details \code{get_n_generated} returns the number of arrivals generated by
#' the given sources. \code{get_trajectory} returns the trajectory to which they
#' are attached (as a list).
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
get_n_generated <- function(.env, sources) UseMethod("get_n_generated")

#' @export
get_n_generated.simmer <- function(.env, sources) {
  get_n_generated_(.env$sim_obj, sources)
}

#' @rdname get_n_generated
#' @export
get_trajectory <- function(.env, sources) UseMethod("get_trajectory")

#' @export
get_trajectory.simmer <- function(.env, sources) {
  lapply(get_trajectory_(.env$sim_obj, sources), "[")
}

#' @rdname get_n_generated
#' @export
get_name <- function(.env) UseMethod("get_name")

#' @export
get_name.simmer <- function(.env) get_name_(.env$sim_obj)

#' @param keys the attribute name(s).
#' @inheritParams set_attribute
#'
#' @rdname get_n_generated
#' @export
get_attribute <- function(.env, keys) UseMethod("get_attribute")

#' @export
get_attribute.simmer <- function(.env, keys) {
  get_attribute_(.env$sim_obj, keys, FALSE)
}

#' @rdname get_n_generated
#' @export
get_global <- function(.env, keys) UseMethod("get_global")

#' @export
get_global.simmer <- function(.env, keys) {
  get_attribute_(.env$sim_obj, keys, TRUE)
}

#' @rdname get_n_generated
#' @export
get_prioritization <- function(.env) UseMethod("get_prioritization")

#' @export
get_prioritization.simmer <- function(.env) get_prioritization_(.env$sim_obj)

#' Get Resource Parameters
#'
#' Getters for resources: server capacity/count and queue size/count, seized
#' amount and selected resources.
#'
#' @inheritParams reset
#' @inheritParams select
#' @param resources one or more resource names.
#'
#' @return Return a vector (character for \code{get_selected}, numeric for the
#' rest of them).
#' @seealso \code{\link{get_resources}}, \code{\link{set_capacity}},
#' \code{\link{set_queue_size}}.
#' @export
get_capacity <- function(.env, resources) UseMethod("get_capacity")

#' @export
get_capacity.simmer <- function(.env, resources) {
  ret <- get_capacity_(.env$sim_obj, resources)
  replace(ret, ret < 0, Inf)
}

#' @rdname get_capacity
#' @export
get_capacity_selected <- function(.env, id=0) UseMethod("get_capacity_selected")

#' @export
get_capacity_selected.simmer <- function(.env, id=0) {
  ret <- get_capacity_selected_(.env$sim_obj, id)
  replace(ret, ret < 0, Inf)
}

#' @rdname get_capacity
#' @export
get_queue_size <- function(.env, resources) UseMethod("get_queue_size")

#' @export
get_queue_size.simmer <- function(.env, resources) {
  ret <- get_queue_size_(.env$sim_obj, resources)
  replace(ret, ret < 0, Inf)
}

#' @rdname get_capacity
#' @export
get_queue_size_selected <- function(.env, id=0) UseMethod("get_queue_size_selected")

#' @export
get_queue_size_selected.simmer <- function(.env, id=0) {
  ret <- get_queue_size_selected_(.env$sim_obj, id)
  replace(ret, ret < 0, Inf)
}

#' @rdname get_capacity
#' @export
get_server_count <- function(.env, resources) UseMethod("get_server_count")

#' @export
get_server_count.simmer <- function(.env, resources) {
  get_server_count_(.env$sim_obj, resources)
}

#' @rdname get_capacity
#' @export
get_server_count_selected <- function(.env, id=0) UseMethod("get_server_count_selected")

#' @export
get_server_count_selected.simmer <- function(.env, id=0) {
  get_server_count_selected_(.env$sim_obj, id)
}

#' @rdname get_capacity
#' @export
get_queue_count <- function(.env, resources) UseMethod("get_queue_count")

#' @export
get_queue_count.simmer <- function(.env, resources) {
  get_queue_count_(.env$sim_obj, resources)
}

#' @rdname get_capacity
#' @export
get_queue_count_selected <- function(.env, id=0) UseMethod("get_queue_count_selected")

#' @export
get_queue_count_selected.simmer <- function(.env, id=0) {
  get_queue_count_selected_(.env$sim_obj, id)
}

#' @rdname get_capacity
#' @export
get_seized <- function(.env, resources) UseMethod("get_seized")

#' @export
get_seized.simmer <- function(.env, resources) {
  get_seized_(.env$sim_obj, resources)
}

#' @rdname get_capacity
#' @export
get_seized_selected <- function(.env, id=0) UseMethod("get_seized_selected")

#' @export
get_seized_selected.simmer <- function(.env, id=0) {
  get_seized_selected_(.env$sim_obj, id)
}

#' @rdname get_capacity
#' @export
get_selected <- function(.env, id=0) UseMethod("get_selected")

#' @export
get_selected.simmer <- function(.env, id=0) get_selected_(.env$sim_obj, id)
