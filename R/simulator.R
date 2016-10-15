#' @useDynLib simmer
#' @importFrom R6 R6Class
#' @importFrom Rcpp evalCpp
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
      else if (inherits(capacity, "simmer.schedule")) {
        capacity_schedule <- capacity
        capacity <- capacity_schedule$get_schedule()$init
      }

      if (is.numeric(queue_size) && is.infinite(queue_size))
        queue_size <- -1
      else if (inherits(queue_size, "simmer.schedule")) {
        queue_size_schedule <- queue_size
        queue_size <- queue_size_schedule$get_schedule()$init
      }

      ret <- add_resource_(private$sim_obj, name, capacity, queue_size, mon,
                           preemptive, preempt_order, queue_size_strict)
      if (ret) private$res[[name]] <- mon

      if (inherits(capacity_schedule, "simmer.schedule"))
        add_resource_manager_(private$sim_obj, name, "capacity",
                              capacity_schedule$get_schedule()$intervals,
                              capacity_schedule$get_schedule()$values,
                              capacity_schedule$get_schedule()$period)
      if (inherits(queue_size_schedule, "simmer.schedule"))
        add_resource_manager_(private$sim_obj, name, "queue_size",
                              queue_size_schedule$get_schedule()$intervals,
                              queue_size_schedule$get_schedule()$values,
                              queue_size_schedule$get_schedule()$period)
      self
    },

    add_generator = function(name_prefix, trajectory, distribution, mon=1,
                             priority=0, preemptible=priority, restart=FALSE) {
      if (!inherits(trajectory, "simmer.trajectory"))
        stop("not a trajectory")
      private$traj <- c(private$traj, trajectory)
      name_prefix <- evaluate_value(name_prefix)
      mon <- evaluate_value(mon)
      priority <- evaluate_value(priority)
      preemptible <- evaluate_value(preemptible)
      restart <- evaluate_value(restart)

      init <- as.list(environment(distribution))
      environment(distribution)$.reset <- new.env(parent = environment(distribution))
      environment(distribution)$.reset$init <- init
      environment(distribution)$.reset$reset <- function() {
        lst <- parent.env(environment())$init
        cls <- parent.env(parent.env(environment()))
        for (i in ls(lst, all.names = TRUE)) assign(i, get(i, lst), cls)
      }
      environment(environment(distribution)$.reset$reset) <- environment(distribution)$.reset

      ret <- add_generator_(private$sim_obj, name_prefix, trajectory$get_head(), distribution, mon,
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
    gen = NULL,
    traj = NULL
  )
)

#' Create a simulator
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
#' t0 <- create_trajectory("my trajectory") %>%
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
#'   add_generator("patient", t0, function() rnorm(1, 10, 2))
#'
#' env %>% run(until=80)
#'
#' plot_resource_usage(env, "doctor")
simmer <- function(name="anonymous", verbose=FALSE) Simmer$new(name, verbose)

#' Reset a simulator
#'
#' Reset the following components of a simulation environment:
#' time, event queue, resources, generators and statistics.
#'
#' @param env the simulation environment.
#'
#' @return Returns the simulation environment.
#' @seealso \code{\link{onestep}}, \code{\link{run}}.
#' @export
reset <- function(env) env$reset()

#' Run the simulation
#'
#' Execute steps until the given criterion.
#'
#' @inheritParams reset
#' @param until stop time.
#'
#' @return Returns the simulation environment.
#' @seealso \code{\link{reset}}.
#' @export
run <- function(env, until=1000) env$run(until)

#' @rdname run
#' @export
onestep <- function(env) env$step()

#' Get current time
#'
#' Get the current simulation time.
#'
#' @inheritParams reset
#'
#' @return Returns a numeric value.
#' @seealso \code{\link{peek}}.
#' @export
now <- function(env) env$now()

#' Peek next events
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
peek <- function(env, steps=1, verbose=FALSE) env$peek(steps, verbose)

#' Add a resource
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
add_resource <- function(env, name, capacity=1, queue_size=Inf, mon=TRUE, preemptive=FALSE,
                         preempt_order=c("fifo", "lifo"), queue_size_strict=FALSE)
  env$add_resource(name, capacity, queue_size, mon, preemptive, preempt_order, queue_size_strict)

#' Add a generator
#'
#' Define a new generator of arrivals in a simulation environment.
#'
#' @inheritParams reset
#' @param name_prefix the name prefix of the generated arrivals.
#' @param trajectory the trajectory that the generated arrivals will follow (see
#' \code{\link{create_trajectory}}).
#' @param distribution a function modelling the interarrival times (returning a
#' negative value stops the generator).
#' @param mon whether the simulator must monitor the generated arrivals or not
#' (0 = no monitoring, 1 = simple arrival monitoring, 2 = level 1 + arrival
#' attribute montoring)
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
add_generator <- function(env, name_prefix, trajectory, distribution, mon=1,
                          priority=0, preemptible=priority, restart=FALSE)
  env$add_generator(name_prefix, trajectory, distribution, mon, priority, preemptible, restart)

#' Get statistics
#'
#' Simulator getters for obtaining monitored data (if any) about arrivals, attributes and resources.
#'
#' @param envs the simulation environment (or a list of environments).
#' @param per_resource if \code{TRUE}, statistics will be reported on a per-resource basis.
#' @param ongoing if \code{TRUE}, ongoing arrivals will be reported. The columns
#' \code{end_time} and \code{finished} of these arrivals are reported as \code{NA}s.
#'
#' @return Returns a data frame.
#' @name get_mon
#' @export
get_mon_arrivals <- function(envs, per_resource=FALSE, ongoing=FALSE)
  envs_apply(envs, "get_mon_arrivals", per_resource, ongoing)

#' @rdname get_mon
#' @export
get_mon_attributes <- function(envs) envs_apply(envs, "get_mon_attributes")

#' @param data whether to retrieve the "counts", the "limits" or both.
#' @rdname get_mon
#' @export
get_mon_resources <- function(envs, data=c("counts", "limits")) envs_apply(envs, "get_mon_resources", data)

#' Get the number of arrivals generated
#'
#' Simulator getter for obtaining the number of arrivals generated by a generator by name.
#'
#' @inheritParams reset
#' @param generator the name of the generator.
#'
#' @return Returns a numeric value.
#' @export
get_n_generated <- function(env, generator) env$get_n_generated(generator)

#' Get a resource's parameter
#'
#' Getters for a resource's server capacity/count and queue size/count.
#'
#' @inheritParams reset
#' @param resource the name of the resource.
#'
#' @return Returns a numeric value.
#' @export
get_capacity <- function(env, resource) env$get_capacity(resource)

#' @rdname get_capacity
#' @export
get_queue_size <- function(env, resource) env$get_queue_size(resource)

#' @rdname get_capacity
#' @export
get_server_count <- function(env, resource) env$get_server_count(resource)

#' @rdname get_capacity
#' @export
get_queue_count <- function(env, resource) env$get_queue_count(resource)
