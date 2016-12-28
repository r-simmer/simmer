Simmer <- R6Class("simmer",
  public = list(
    name = NA,

    initialize = function(name="anonymous", verbose=FALSE) {
      self$name <- evaluate_value(name)
      private$sim_obj <- Simulator__new(name, evaluate_value(verbose))
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
          " | monitored: ", private$res[[name]],
          " | server status: ", self$get_server_count(name),
          "(", self$get_capacity(name), ")",
          " | queue status: ", self$get_queue_count(name),
          "(", self$get_queue_size(name), ") }\n"
        ))
      for (name in names(private$gen))
        cat(paste0(
          "{ Generator: ", name,
          " | monitored: ", private$gen[[name]],
          " | n_generated: ", self$get_n_generated(name), " }\n"
        ))
    },

    reset = function() {
      reset_(private$sim_obj)
      self
    },

    now = function() { now_(private$sim_obj) },

    peek = function(steps=1, verbose=FALSE) {
      steps <- evaluate_value(steps)
      verbose <- evaluate_value(verbose)
      steps <- replace(steps, steps == Inf, -1)
      ret <- as.data.frame(peek_(private$sim_obj, steps))
      if (!verbose) ret$time
      else ret # nocov
    },

    step = function() {
      step_(private$sim_obj)
      self
    },

    run = function(until=1000) {
      until <- evaluate_value(until)
      until <- replace(until, until == Inf, -1)
      run_(private$sim_obj, until)
      self
    },

    add_resource = function(name, capacity=1, queue_size=Inf, mon=TRUE, preemptive=FALSE,
                            preempt_order=c("fifo", "lifo"), queue_size_strict=FALSE) {
      preempt_order <- match.arg(preempt_order)
      name <- evaluate_value(name)
      capacity <- evaluate_value(capacity)
      capacity_schedule <- NA
      queue_size <- evaluate_value(queue_size)
      queue_size_schedule <- NA
      mon <- evaluate_value(mon)
      preemptive <- evaluate_value(preemptive)
      queue_size_strict <- evaluate_value(queue_size_strict)

      if (is.numeric(capacity) && is.infinite(capacity))
        capacity <- -1
      else if (inherits(capacity, "schedule")) {
        capacity_schedule <- capacity
        capacity <- capacity_schedule$get_schedule()$init
      }

      if (is.numeric(queue_size) && is.infinite(queue_size))
        queue_size <- -1
      else if (inherits(queue_size, "schedule")) {
        queue_size_schedule <- queue_size
        queue_size <- queue_size_schedule$get_schedule()$init
      }

      ret <- add_resource_(private$sim_obj, name, capacity, queue_size, mon,
                           preemptive, preempt_order, queue_size_strict)
      if (ret) private$res[[name]] <- mon

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
                             priority=0, preemptible=priority, restart=FALSE) {
      stopifnot(inherits(trajectory, "trajectory"))
      name_prefix <- evaluate_value(name_prefix)
      mon <- evaluate_value(mon)
      priority <- evaluate_value(priority)
      preemptible <- evaluate_value(preemptible)
      restart <- evaluate_value(restart)
      distribution <- make_resetable(distribution)
      ret <- add_generator_(private$sim_obj, name_prefix, trajectory[], distribution, mon,
                            priority, preemptible, restart)
      if (ret) private$gen[[name_prefix]] <- mon
      self
    },

    get_mon_arrivals = function(per_resource=FALSE, ongoing=FALSE) {
      per_resource <- evaluate_value(per_resource)
      ongoing <- evaluate_value(ongoing)
      as.data.frame(
        get_mon_arrivals_(private$sim_obj, per_resource, ongoing),
        stringsAsFactors = FALSE
      )
    },

    get_mon_attributes = function()
      as.data.frame(get_mon_attributes_(private$sim_obj), stringsAsFactors = FALSE),

    get_mon_resources = function(data=c("counts", "limits")) {
      data <- match.arg(data, several.ok = TRUE)
      monitor_data <- as.data.frame(
        if (identical(data, "counts"))
          get_mon_resource_counts_(private$sim_obj)
        else if (identical(data, "limits"))
          get_mon_resource_limits_(private$sim_obj)
        else
          get_mon_resource_(private$sim_obj)
        , stringsAsFactors = FALSE)
      if (identical(data, "limits")) {
        monitor_data$server <-
          replace(monitor_data$server, monitor_data$server == -1, Inf)
        monitor_data$queue <-
          replace(monitor_data$queue, monitor_data$queue == -1, Inf)
        monitor_data$system <- monitor_data$server + monitor_data$queue
      } else if (all(c("counts", "limits") %in% data)) {
        monitor_data$capacity <-
          replace(monitor_data$capacity, monitor_data$capacity == -1, Inf)
        monitor_data$queue_size <-
          replace(monitor_data$queue_size, monitor_data$queue_size == -1, Inf)
        monitor_data$system <- monitor_data$server + monitor_data$queue
        monitor_data$limit <- monitor_data$capacity + monitor_data$queue_size
      } else monitor_data$system <- monitor_data$server + monitor_data$queue
      monitor_data
    },

    get_n_generated = function(name) {
      get_n_generated_(private$sim_obj, evaluate_value(name))
    },

    get_capacity = function(name) {
      ret <- get_capacity_(private$sim_obj, evaluate_value(name))
      if (ret < 0) ret <- Inf
      ret
    },

    get_queue_size = function(name) {
      ret <- get_queue_size_(private$sim_obj, evaluate_value(name))
      if (ret < 0) ret <- Inf
      ret
    },

    get_server_count = function(name) {
      get_server_count_(private$sim_obj, evaluate_value(name))
    },

    get_queue_count = function(name) {
      get_queue_count_(private$sim_obj, evaluate_value(name))
    },

    # not exposed, internal use
    get_generators = function() { private$gen },
    get_resources = function() { private$res }
  ),

  private = list(
    sim_obj = NULL,
    res = NULL,
    gen = NULL
  )
)
