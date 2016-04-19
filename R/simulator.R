#' @useDynLib simmer
#' @importFrom R6 R6Class
#' @importFrom Rcpp evalCpp
Simmer <- R6Class("Simmer",
  public = list(
    name = NA,
    
    initialize = function(name="anonymous", verbose=FALSE) {
      self$name <- evaluate_value(name)
      private$sim_obj <- Simulator__new(name, evaluate_value(verbose))
      self
    },
    
    print = function() {
      cat(paste0(
        "Simmer environment: ", self$name,
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
    
    peek = function() {
      ret <- peek_(private$sim_obj)
      if (ret >= 0) ret
      else Inf
    },
    
    step = function() {
      step_(private$sim_obj)
      self
    },
    
    run = function(until=1000) {
      until <- evaluate_value(until)
      if(!is.finite(until)) until <- 1000
      
      run_(private$sim_obj, until)
      self
    },
    
    add_resource = function(name, capacity=1, queue_size=Inf, mon=TRUE,
                            preemptive=FALSE, preempt_order="fifo") {
      if (!preempt_order %in% c("fifo", "lifo"))
        stop("preempt order '", preempt_order, "' not supported")
      name <- evaluate_value(name)
      capacity <- evaluate_value(capacity)
      capacity_schedule <- NA
      queue_size <- evaluate_value(queue_size)
      queue_size_schedule <- NA
      mon <- evaluate_value(mon)
      preemptive <- evaluate_value(preemptive)
      preempt_order <- evaluate_value(preempt_order)
      
      if (is.numeric(capacity) && is.infinite(capacity))
        capacity <- -1
      else if (inherits(capacity, "Schedule")) {
        capacity_schedule <- capacity
        capacity <- capacity_schedule$get_init()
      }
      
      if (is.numeric(queue_size) && is.infinite(queue_size))
        queue_size <- -1
      else if (inherits(queue_size, "Schedule")) {
        queue_size_schedule <- queue_size
        queue_size <- queue_size_schedule$get_init()
      }
      
      ret <- add_resource_(private$sim_obj, name, capacity, queue_size, mon,
                           preemptive, preempt_order)
      if (ret) private$res[[name]] <- mon
      
      if (inherits(capacity_schedule, "Schedule"))
        add_resource_manager_(private$sim_obj, name, "capacity",
                              capacity_schedule$get_schedule()$intervals,
                              capacity_schedule$get_schedule()$values)
      if (inherits(queue_size_schedule, "Schedule"))
        add_resource_manager_(private$sim_obj, name, "queue_size",
                              queue_size_schedule$get_schedule()$intervals,
                              queue_size_schedule$get_schedule()$values)
      self
    },
    
    add_generator = function(name_prefix, trajectory, dist, mon=1) {
      if (!inherits(trajectory, "Trajectory"))
        stop("not a trajectory")
      name_prefix <- evaluate_value(name_prefix)
      mon <- evaluate_value(mon)

      ret <- add_generator_(private$sim_obj, name_prefix, trajectory$get_head(), dist, mon)
      if (ret) private$gen[[name_prefix]] <- mon
      self
    },
    
    get_mon_arrivals = function(per_resource=FALSE) {
      if (per_resource) {
        if (sum(private$gen>0))
          do.call(rbind, lapply(names(private$gen[private$gen>0]), function(i) {
            monitor_data <- as.data.frame(
              get_mon_arrivals_per_resource_(private$sim_obj, i)
            )
          }))
        else data.frame(name = character(),
                        start_time = numeric(),
                        end_time = numeric(), 
                        activity_time = numeric(), 
                        resource = character())
      } else {
        if (sum(private$gen>0))
          do.call(rbind, lapply(names(private$gen[private$gen>0]), function(i) {
            monitor_data <- as.data.frame(
              get_mon_arrivals_(private$sim_obj, i)
            )
          }))
        else data.frame(name = character(),
                        start_time = numeric(),
                        end_time = numeric(), 
                        activity_time = numeric(), 
                        finished = logical())
      }
    },
    
    get_mon_attributes = function() {
      if (sum(private$gen>1))
        do.call(rbind, lapply(names(private$gen[private$gen>1]), function(i) {
          monitor_data <- as.data.frame(
            get_mon_attributes_(private$sim_obj, i)
          )
        }))
      else data.frame(time = numeric(),
                      name = character(),
                      key = character(),
                      value = numeric())
    },
    
    get_mon_resources = function(data="counts") {
      if (all(!data %in% c("counts", "limits")))
        stop("parameter 'data' should be 'counts', 'limits' or both")
      if (sum(private$res>0))
        do.call(rbind,
          lapply(names(private$res[private$res>0]), function(i) {
            monitor_data <- as.data.frame(
              if (all(data %in% "counts"))
                get_mon_resource_counts_(private$sim_obj, i)
              else if (all(data %in% "limits"))
                get_mon_resource_limits_(private$sim_obj, i)
              else
                get_mon_resource_(private$sim_obj, i)
            )
            tryCatch({
              if (all(data %in% "limits")) {
                monitor_data$server <- 
                  replace(monitor_data$server, monitor_data$server==-1, Inf)
                monitor_data$queue <- 
                  replace(monitor_data$queue, monitor_data$queue==-1, Inf)
                monitor_data$system <- monitor_data$server + monitor_data$queue
              } else if (all(c("counts", "limits") %in% data)) {
                monitor_data$capacity <- 
                  replace(monitor_data$capacity, monitor_data$capacity==-1, Inf)
                monitor_data$queue_size <- 
                  replace(monitor_data$queue_size, monitor_data$queue_size==-1, Inf)
                monitor_data$system <- monitor_data$server + monitor_data$queue
                monitor_data$limit <- monitor_data$capacity + monitor_data$queue_size
              } else monitor_data$system <- monitor_data$server + monitor_data$queue
              monitor_data$resource <- i
            }, error = function(e) {
              monitor_data$system <<- numeric()
              if (all(data %in% c("counts", "limits")))
                monitor_data$limit <<- numeric()
              monitor_data$resource <<- character()
            })
            monitor_data
          })
        )
      else {
        monitor_data <- data.frame(time = numeric(),
                                   server = numeric(),
                                   queue = numeric())
        if (all(c("counts", "limits") %in% data)) {
          monitor_data$capacity <- numeric()
          monitor_data$queue_size <- numeric()
          monitor_data$system <- numeric()
          monitor_data$limit <- numeric()
        } else monitor_data$system <- numeric()
        monitor_data$resource <- character()
        monitor_data
      }
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

#' Create a simulator
#'
#' This function initialises a simulation environment.
#' 
#' @param name the name of the simulator.
#' @param verbose enable showing activity information.
#' 
#' @return Returns a simulation environment.
#' @seealso Other methods to deal with a simulation environment:
#' \link{reset}, \link{now}, \link{peek}, \link{onestep}, \link{run}, 
#' \link{add_resource}, \link{add_generator}, \link{get_mon_arrivals}, \link{get_mon_attributes},
#' \link{get_mon_resources}, \link{get_n_generated}, \link{get_capacity}, \link{get_queue_size},
#' \link{get_server_count}, \link{get_queue_count}.
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
#' Resets the following components of a simulation environment: 
#' time, event queue, resources, generators and statistics.
#' 
#' @param env the simulation environment.
#' 
#' @return Returns the simulation environment.
#' @seealso Other methods to deal with a simulation environment:
#' \link{simmer}, \link{now}, \link{peek}, \link{onestep}, \link{run}, 
#' \link{add_resource}, \link{add_generator}, \link{get_mon_arrivals}, \link{get_mon_attributes},
#' \link{get_mon_resources}, \link{get_n_generated}, \link{get_capacity}, \link{get_queue_size},
#' \link{get_server_count}, \link{get_queue_count}.
#' @export
reset <- function(env) env$reset()

#' Get the current time
#'
#' Gets the current simulation time.
#' 
#' @param env the simulation environment.
#' 
#' @return Returns a numeric value.
#' @seealso Other methods to deal with a simulation environment:
#' \link{simmer}, \link{reset}, \link{peek}, \link{onestep}, \link{run}, 
#' \link{add_resource}, \link{add_generator}, \link{get_mon_arrivals}, \link{get_mon_attributes},
#' \link{get_mon_resources}, \link{get_n_generated}, \link{get_capacity}, \link{get_queue_size},
#' \link{get_server_count}, \link{get_queue_count}.
#' @export
now <- function(env) env$now()

#' Peek the next event's time
#'
#' Gets the time of the next scheduled event.
#' 
#' @param env the simulation environment.
#' 
#' @return Returns a numeric value.
#' @seealso Other methods to deal with a simulation environment:
#' \link{simmer}, \link{reset}, \link{now}, \link{onestep}, \link{run}, 
#' \link{add_resource}, \link{add_generator}, \link{get_mon_arrivals}, \link{get_mon_attributes},
#' \link{get_mon_resources}, \link{get_n_generated}, \link{get_capacity}, \link{get_queue_size},
#' \link{get_server_count}, \link{get_queue_count}.
#' @export
peek <- function(env) env$peek()

#' Step the simulation
#'
#' Processes the next event.
#' 
#' @param env the simulation environment.
#' 
#' @return Returns the simulation environment.
#' @seealso Other methods to deal with a simulation environment:
#' \link{simmer}, \link{reset}, \link{now}, \link{peek}, \link{run}, 
#' \link{add_resource}, \link{add_generator}, \link{get_mon_arrivals}, \link{get_mon_attributes},
#' \link{get_mon_resources}, \link{get_n_generated}, \link{get_capacity}, \link{get_queue_size},
#' \link{get_server_count}, \link{get_queue_count}.
#' @export
onestep <- function(env) env$step()

#' Run the simulation
#'
#' Executes steps until the given criterion.
#' 
#' @param env the simulation environment.
#' @param until stop time.
#' 
#' @return Returns the simulation environment.
#' @seealso Other methods to deal with a simulation environment:
#' \link{simmer}, \link{reset}, \link{now}, \link{peek}, \link{onestep}, 
#' \link{add_resource}, \link{add_generator}, \link{get_mon_arrivals}, \link{get_mon_attributes},
#' \link{get_mon_resources}, \link{get_n_generated}, \link{get_capacity}, \link{get_queue_size},
#' \link{get_server_count}, \link{get_queue_count}.
#' @export
run <- function(env, until=1000) env$run(until)

#' Add a resource
#'
#' Adds a resource to a simulation environment.
#' 
#' @param env the simulation environment.
#' @param name the name of the resource.
#' @param capacity the capacity of the server.
#' @param queue_size the size of the queue.
#' @param mon whether the simulator must monitor this resource or not.
#' @param preemptive whether arrivals in the server can be preempted or not based
#' on seize priorities.
#' @param preempt_order if the resource is preemptive and preemption occurs with 
#' more than one arrival in the server, this parameter defines which arrival should 
#' be preempted first. It must be `fifo` (First In First Out: older preemptible 
#' tasks are preempted first) or `lifo` (Last In First Out: newer preemptible tasks 
#' are preempted first).
#' 
#' @return Returns the simulation environment.
#' @seealso Other methods to deal with a simulation environment:
#' \link{simmer}, \link{reset}, \link{now}, \link{peek}, \link{onestep}, \link{run}, 
#' \link{add_generator}, \link{get_mon_arrivals}, \link{get_mon_attributes},
#' \link{get_mon_resources}, \link{get_n_generated}, \link{get_capacity}, \link{get_queue_size},
#' \link{get_server_count}, \link{get_queue_count}.
#' @export
add_resource <- function(env, name, capacity=1, queue_size=Inf, mon=TRUE,
                         preemptive=FALSE, preempt_order="fifo")
  env$add_resource(name, capacity, queue_size, mon, preemptive, preempt_order)

#' Add a generator
#'
#' Adds a generator to a simulation environment.
#' 
#' @param env the simulation environment.
#' @param name_prefix the name prefix of the generated arrivals.
#' @param trajectory the trajectory that the generated arrivals will follow (see \link{create_trajectory}).
#' @param dist a function modelling the interarrival times (returning a negative value stops the generator).
#' @param mon whether the simulator must monitor the generated arrivals or not (0=no monitoring, 1=simple arrival monitoring, 2=level 1 + arrival attribute montoring)
#' 
#' @return Returns the simulation environment.
#' @seealso Other methods to deal with a simulation environment:
#' \link{simmer}, \link{reset}, \link{now}, \link{peek}, \link{onestep}, \link{run}, 
#' \link{add_resource}, \link{get_mon_arrivals}, \link{get_mon_attributes},
#' \link{get_mon_resources}, \link{get_n_generated}, \link{get_capacity}, \link{get_queue_size},
#' \link{get_server_count}, \link{get_queue_count}.
#' @export
add_generator <- function(env, name_prefix, trajectory, dist, mon=1)
  env$add_generator(name_prefix, trajectory, dist, mon)

#' Get arrival statistics
#'
#' Gets the arrivals' monitored data (if any).
#' 
#' @param env the simulation environment (or a list of environments).
#' @param per_resource whether the activity should be reported on a per-resource basis (by default: FALSE).
#' 
#' @return Returns a data frame.
#' @seealso Other methods to deal with a simulation environment:
#' \link{simmer}, \link{reset}, \link{now}, \link{peek}, \link{onestep}, \link{run}, 
#' \link{add_resource}, \link{add_generator}, \link{get_mon_attributes},
#' \link{get_mon_resources}, \link{get_n_generated}, \link{get_capacity}, \link{get_queue_size},
#' \link{get_server_count}, \link{get_queue_count}.
#' @export
get_mon_arrivals <- function(env, per_resource=FALSE) 
  envs_apply(env, "get_mon_arrivals", per_resource)

#' Get attribute statistics
#'
#' Gets the arrivals' attributes over time (if any).
#' 
#' @param env the simulation environment (or a list of environments).
#' 
#' @return Returns a data frame.
#' @seealso Other methods to deal with a simulation environment:
#' \link{simmer}, \link{reset}, \link{now}, \link{peek}, \link{onestep}, \link{run}, 
#' \link{add_resource}, \link{add_generator}, \link{get_mon_arrivals},
#' \link{get_mon_resources}, \link{get_n_generated}, \link{get_capacity}, \link{get_queue_size},
#' \link{get_server_count}, \link{get_queue_count}.
#' @export
get_mon_attributes <- function(env) envs_apply(env, "get_mon_attributes")

#' Get resource statistics
#'
#' Gets the resources' monitored data (if any).
#' 
#' @param env the simulation environment (or a list of environments).
#' @param data whether to retrieve the "counts", the "limits" or both.
#' 
#' @return Returns a data frame.
#' @seealso Other methods to deal with a simulation environment:
#' \link{simmer}, \link{reset}, \link{now}, \link{peek}, \link{onestep}, \link{run}, 
#' \link{add_resource}, \link{add_generator}, \link{get_mon_arrivals}, \link{get_mon_attributes},
#' \link{get_n_generated}, \link{get_capacity}, \link{get_queue_size},
#' \link{get_server_count}, \link{get_queue_count}.
#' @export
get_mon_resources <- function(env, data="counts") envs_apply(env, "get_mon_resources", data)

#' Get the number of arrivals generated
#'
#' Gets the number of arrivals generated by a generator by name.
#' 
#' @param env the simulation environment.
#' @param name the name of the generator.
#' 
#' @return Returns a numeric value.
#' @seealso Other methods to deal with a simulation environment:
#' \link{simmer}, \link{reset}, \link{now}, \link{peek}, \link{onestep}, \link{run}, 
#' \link{add_resource}, \link{add_generator}, \link{get_mon_arrivals}, \link{get_mon_attributes},
#' \link{get_mon_resources}, \link{get_capacity}, \link{get_queue_size},
#' \link{get_server_count}, \link{get_queue_count}.
#' @export
get_n_generated <- function(env, name) env$get_n_generated(name)

#' Get the capacity
#'
#' Gets the capacity of a resource by name.
#' 
#' @param env the simulation environment.
#' @param name the name of the resource.
#' 
#' @return Returns a numeric value.
#' @seealso Other methods to deal with a simulation environment:
#' \link{simmer}, \link{reset}, \link{now}, \link{peek}, \link{onestep}, \link{run}, 
#' \link{add_resource}, \link{add_generator}, \link{get_mon_arrivals}, \link{get_mon_attributes},
#' \link{get_mon_resources}, \link{get_n_generated}, \link{get_queue_size},
#' \link{get_server_count}, \link{get_queue_count}.
#' @export
get_capacity <- function(env, name) env$get_capacity(name)

#' Get the queue size
#'
#' Gets the queue size of a resource by name.
#' 
#' @param env the simulation environment.
#' @param name the name of the resource.
#' 
#' @return Returns a numeric value.
#' @seealso Other methods to deal with a simulation environment:
#' \link{simmer}, \link{reset}, \link{now}, \link{peek}, \link{onestep}, \link{run}, 
#' \link{add_resource}, \link{add_generator}, \link{get_mon_arrivals}, \link{get_mon_attributes},
#' \link{get_mon_resources}, \link{get_n_generated}, \link{get_capacity},
#' \link{get_server_count}, \link{get_queue_count}.
#' @export
get_queue_size <- function(env, name) env$get_queue_size(name)

#' Get the server count
#'
#' Gets the number of customers in a resource's server by name.
#' 
#' @param env the simulation environment.
#' @param name the name of the resource.
#' 
#' @return Returns a numeric value.
#' @seealso Other methods to deal with a simulation environment:
#' \link{simmer}, \link{reset}, \link{now}, \link{peek}, \link{onestep}, \link{run}, 
#' \link{add_resource}, \link{add_generator}, \link{get_mon_arrivals}, \link{get_mon_attributes},
#' \link{get_mon_resources}, \link{get_n_generated}, \link{get_capacity}, \link{get_queue_size},
#' \link{get_queue_count}.
#' @export
get_server_count <- function(env, name) env$get_server_count(name)

#' Get the queue count
#'
#' Gets the number of customers in a resource's queue by name.
#' 
#' @param env the simulation environment.
#' @param name the name of the resource.
#' 
#' @return Returns a numeric value.
#' @seealso Other methods to deal with a simulation environment:
#' \link{simmer}, \link{reset}, \link{now}, \link{peek}, \link{onestep}, \link{run}, 
#' \link{add_resource}, \link{add_generator}, \link{get_mon_arrivals}, \link{get_mon_attributes},
#' \link{get_mon_resources}, \link{get_n_generated}, \link{get_capacity}, \link{get_queue_size},
#' \link{get_server_count}.
#' @export
get_queue_count <- function(env, name) env$get_queue_count(name)
