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

Simmer <- R6Class("simmer",
  public = list(
    name = NA,

    initialize = function(name="anonymous", verbose=FALSE, mon=monitor_mem(), log_level=0) {
      check_args(name="string", verbose="flag", mon="monitor", log_level="number")
      self$name <- name
      private$mon <- mon
      private$sim_obj <- Simulator__new(name, verbose, mon$get_xptr(), log_level)
      self
    },

    print = function() {
      cat(paste0(
        "simmer environment: ", self$name,
        " | now: ", self$now(), " | next: ", self$peek(), "\n",
        "{ Monitor: ", private$mon$name, " }\n"
      ))
      for (name in names(private$mon$handlers)) cat(paste0(
        "  { ", name, ": ", private$mon$handlers[[name]], " }\n"
      ))
      for (name in names(private$resources)) cat(paste0(
        "{ Resource: ", name,
        " | monitored: ", private$resources[[name]][["mon"]],
        " | server status: ", self$get_server_count(name),
        "(", self$get_capacity(name), ")",
        " | queue status: ", self$get_queue_count(name),
        "(", self$get_queue_size(name), ") }\n"
      ))
      for (name in names(private$sources)) cat(paste0(
        "{ Source: ", name,
        " | monitored: ", private$sources[[name]][["mon"]],
        " | n_generated: ", self$get_n_generated(name), " }\n"
      ))
      for (name in names(private$globals)) {
        value <- private$globals[[name]]
        is_schedule <- inherits(value, "schedule")
        if (is_schedule) value <- value$get_schedule()$init
        cat(paste0(
          "{ Global: ", name, " | schedule: ", is_schedule,
          " | initial value: ", value, " }\n"
        ))
      }
      invisible(self)
    },

    reset = function() {
      reset_(private$sim_obj)
      self
    },

    now = function() { now_(private$sim_obj) },

    peek = function(steps=1, verbose=FALSE) {
      check_args(steps="number", verbose="flag")
      ret <- peek_(private$sim_obj, steps)
      if (!verbose) ret$time
      else ret # nocov
    },

    stepn = function(n=1) {
      check_args(n="number")
      stepn_(private$sim_obj, n)
      self
    },

    run = function(until=Inf) {
      run_(private$sim_obj, until)
      self
    },

    add_resource = function(name, capacity=1, queue_size=Inf, mon=TRUE, preemptive=FALSE,
                            preempt_order=c("fifo", "lifo"), queue_size_strict=FALSE)
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
        capacity <- capacity_schedule$get_schedule()$init
      } else capacity_schedule <- NA

      if (inherits(queue_size, "schedule")) {
        queue_size_schedule <- queue_size
        queue_size <- queue_size_schedule$get_schedule()$init
      } else queue_size_schedule <- NA

      ret <- add_resource_(private$sim_obj, name, capacity, queue_size, mon,
                           preemptive, preempt_order, queue_size_strict)
      if (ret) private$resources[[name]] <- c(mon=mon, preemptive=preemptive)

      if (inherits(capacity_schedule, "schedule"))
        add_resource_manager_(private$sim_obj, name, "capacity",
                              capacity_schedule$get_schedule()$intervals,
                              capacity_schedule$get_schedule()$values,
                              capacity_schedule$get_schedule()$period)
      if (inherits(queue_size_schedule, "schedule"))
        add_resource_manager_(private$sim_obj, name, "queue_size",
                              queue_size_schedule$get_schedule()$intervals,
                              queue_size_schedule$get_schedule()$values,
                              queue_size_schedule$get_schedule()$period)
      self
    },

    add_generator = function(name_prefix, trajectory, distribution, mon=1,
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
      ret <- add_generator_(private$sim_obj, name_prefix, trajectory[],
                            make_resetable(distribution), mon, priority, preemptible, restart)
      if (ret) private$sources[[name_prefix]] <- c(mon=mon)
      self
    },

    add_dataframe = function(name_prefix, trajectory, data, mon=1, batch=50,
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

      ret <- add_dataframe_(private$sim_obj, name_prefix, trajectory[], data, mon, batch,
                            col_time, col_attributes, col_priority, col_preemptible, col_restart)
      if (ret) private$sources[[name_prefix]] <- c(mon=mon)
      self
    },

    add_global = function(key, value) {
      check_args(key = "string", value = c("numeric", "schedule"))

      ret <- if (inherits(value, "schedule"))
        add_global_manager_(private$sim_obj, key, value$get_schedule()$intervals,
                            value$get_schedule()$values, value$get_schedule()$period)
      else add_global_manager_(private$sim_obj, key, 0, value, -1)

      if (ret) private$globals[[key]] <- value
      self
    },

    get_mon_arrivals = function(per_resource=FALSE, ongoing=FALSE) {
      if (ongoing) record_ongoing_(private$sim_obj, per_resource)
      private$mon$get_arrivals(per_resource)
    },

    get_mon_attributes = function() private$mon$get_attributes(),

    get_mon_resources = function() {
      monitor_data <- private$mon$get_resources()
      monitor_data$capacity <-
        replace(monitor_data$capacity, monitor_data$capacity == -1, Inf)
      monitor_data$queue_size <-
        replace(monitor_data$queue_size, monitor_data$queue_size == -1, Inf)
      monitor_data$system <- monitor_data$server + monitor_data$queue
      monitor_data$limit <- monitor_data$capacity + monitor_data$queue_size
      monitor_data
    },

    get_n_generated = function(sources) get_n_generated_(private$sim_obj, sources),

    get_trajectory = function(sources)
      lapply(get_trajectory_(private$sim_obj, sources), "["),

    get_name = function() get_name_(private$sim_obj),

    get_attribute = function(keys) get_attribute_(private$sim_obj, keys, FALSE),

    get_global = function(keys) get_attribute_(private$sim_obj, keys, TRUE),

    get_prioritization = function() get_prioritization_(private$sim_obj),

    get_capacity = function(resources, id=0) {
      check_args(resources=c("string vector", "NULL"), id="numeric")
      ret <- switch(
        binarise(is.null(resources)),
        get_capacity_(private$sim_obj, resources),
        get_capacity_selected_(private$sim_obj, id)
      )
      replace(ret, ret < 0, Inf)
    },

    get_queue_size = function(resources, id=0) {
      check_args(resources=c("string vector", "NULL"), id="numeric")
      ret <- switch(
        binarise(is.null(resources)),
        get_queue_size_(private$sim_obj, resources),
        get_queue_size_selected_(private$sim_obj, id)
      )
      replace(ret, ret < 0, Inf)
    },

    get_server_count = function(resources, id=0) {
      check_args(resources=c("string vector", "NULL"), id="numeric")
      switch(
        binarise(is.null(resources)),
        get_server_count_(private$sim_obj, resources),
        get_server_count_selected_(private$sim_obj, id)
      )
    },

    get_queue_count = function(resources, id=0) {
      check_args(resources=c("string vector", "NULL"), id="numeric")
      ret <- switch(
        binarise(is.null(resources)),
        get_queue_count_(private$sim_obj, resources),
        get_queue_count_selected_(private$sim_obj, id)
      )
    },

    get_selected = function(id=0) {
      check_args(id="numeric")
      get_selected_(private$sim_obj, id)
    },

    get_sources = function() { private$sources },
    get_resources = function() { private$resources },
    get_globals = function() { private$globals }
  ),

  private = list(
    sim_obj = NULL,
    mon = NULL,
    resources = list(),
    sources = list(),
    globals = list()
  )
)
