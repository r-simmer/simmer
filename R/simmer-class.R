Simmer <- R6Class("simmer",
  public = list(
    name = NA,

    initialize = function(name="anonymous", verbose=FALSE) {
      check_args(name="string", verbose="flag")
      self$name <- name
      private$sim_obj <- Simulator__new(name, verbose)
      self
    },

    print = function() {
      cat(paste0(
        "simmer environment: ", self$name,
        " | now: ", self$now(), " | next: ", self$peek(), "\n"
      ))
      for (name in names(private$res))
        cat(paste0(
          "{ Resource: ", name,
          " | monitored: ", private$res[[name]][["mon"]],
          " | server status: ", self$get_server_count(name),
          "(", self$get_capacity(name), ")",
          " | queue status: ", self$get_queue_count(name),
          "(", self$get_queue_size(name), ") }\n"
        ))
      for (name in names(private$src))
        cat(paste0(
          "{ Source: ", name,
          " | monitored: ", private$src[[name]][["mon"]],
          " | n_generated: ", self$get_n_generated(name), " }\n"
        ))
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
      if (ret) private$res[[name]] <- c(mon=mon, preemptive=preemptive)

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
      if (ret) private$src[[name_prefix]] <- c(mon=mon)
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

      if (any(data[[col_time]] < 0))
        stop(get_caller(), ": time must be positive", call.=FALSE)

      if (time == "absolute") {
        if (is.unsorted(data[[col_time]]))
          stop(get_caller(), ": unsorted absolute time provided", call.=FALSE)
        data[[col_time]] <- c(data[[col_time]][1], diff(data[[col_time]]))
      }

      if (!length(col_attributes)) {
        col_attributes <- setdiff(names(data), col_names)
        col_names <- c(col_names, col_attributes)
      }

      for (col_name in col_names) {
        if (!(is.numeric(data[[col_name]]) || is.logical(data[[col_name]])))
          stop(get_caller(), ": column '", col_name, "' is not numeric", call.=FALSE)
      }

      ret <- add_dataframe_(private$sim_obj, name_prefix, trajectory[], data, mon, batch,
                            col_time, col_attributes, col_priority, col_preemptible, col_restart)
      if (ret) private$src[[name_prefix]] <- c(mon=mon)
      self
    },

    get_mon_arrivals = function(per_resource=FALSE, ongoing=FALSE) {
      get_mon_arrivals_(private$sim_obj, per_resource, ongoing)
    },

    get_mon_attributes = function() get_mon_attributes_(private$sim_obj),

    get_mon_resources = function() {
      monitor_data <- get_mon_resources_(private$sim_obj)
      monitor_data$capacity <-
        replace(monitor_data$capacity, monitor_data$capacity == -1, Inf)
      monitor_data$queue_size <-
        replace(monitor_data$queue_size, monitor_data$queue_size == -1, Inf)
      monitor_data$system <- monitor_data$server + monitor_data$queue
      monitor_data$limit <- monitor_data$capacity + monitor_data$queue_size
      monitor_data
    },

    get_n_generated = function(source) get_n_generated_(private$sim_obj, source),

    get_name = function() get_name_(private$sim_obj),

    get_attribute = function(keys, global=FALSE) get_attribute_(private$sim_obj, keys, global),

    get_prioritization = function() get_prioritization_(private$sim_obj),

    get_capacity = function(resource, id=0) {
      check_args(resource=c("string", "NA"), id="number")
      ret <- switch(
        binarise(is.na(resource)),
        get_capacity_(private$sim_obj, resource),
        get_capacity_selected_(private$sim_obj, id)
      )
      if (ret < 0) ret <- Inf
      ret
    },

    get_queue_size = function(resource, id=0) {
      check_args(resource=c("string", "NA"), id="number")
      ret <- switch(
        binarise(is.na(resource)),
        get_queue_size_(private$sim_obj, resource),
        get_queue_size_selected_(private$sim_obj, id)
      )
      if (ret < 0) ret <- Inf
      ret
    },

    get_server_count = function(resource, id=0) {
      check_args(resource=c("string", "NA"), id="number")
      switch(
        binarise(is.na(resource)),
        get_server_count_(private$sim_obj, resource),
        get_server_count_selected_(private$sim_obj, id)
      )
    },

    get_queue_count = function(resource, id=0) {
      check_args(resource=c("string", "NA"), id="number")
      ret <- switch(
        binarise(is.na(resource)),
        get_queue_count_(private$sim_obj, resource),
        get_queue_count_selected_(private$sim_obj, id)
      )
    },

    # not exposed, internal use
    get_sources = function() { private$src },
    get_resources = function() { private$res }
  ),

  private = list(
    sim_obj = NULL,
    res = list(),
    src = list()
  )
)
