#' Simmer
#'
#' The simulation environment.
#'
#' @seealso \link{Trajectory}
#' @section Methods:
#' \preformatted{## Object creation
#' Simmer$new(name="anonymous", verbose=FALSE)
#' }\describe{
#'   \item{name}{the name of the simulator}
#'   \item{verbose}{enable showing event information}
#' }
#' \preformatted{## Reset the simulator (time, statistics, resources, generators)
#' Simmer$reset() 
#' }\preformatted{## Get the current simulation time
#' Simmer$now()
#' }
#' \preformatted{## Get the time of the next scheduled event
#' Simmer$peek()
#' }
#' \preformatted{## Process the next event
#' Simmer$step()
#' }
#' \preformatted{## Execute steps until the given criterion
#' Simmer$run(until=1000)
#' }\describe{
#'   \item{until}{the stop time}
#' }
#' \preformatted{## Add a resource to the simulation environment
#' Simmer$add_resource(name, capacity=1, queue_size=Inf, mon=TRUE)
#' }\describe{
#'   \item{name}{the name of the resource}
#'   \item{capacity}{the capacity of the server}
#'   \item{queue_size}{the size of the queue}
#'   \item{mon}{whether the simulator must monitor this resource or not}
#' }
#' \preformatted{## Add a generator to the simulation environment
#' Simmer$add_generator(name_prefix, trajectory, dist, mon=TRUE)
#' }\describe{
#'   \item{name_prefix}{the name prefix of the generated arrivals}
#'   \item{trajectory}{the \link{Trajectory} that the generated arrivals will follow}
#'   \item{dist}{a function modelling the interarrival times}
#'   \item{mon}{whether the simulator must monitor the generated arrivals or not}
#' }
#' \preformatted{## Get arrival statistics
#' Simmer$get_mon_arrivals()
#' }
#' \preformatted{## Get resource statistics
#' Simmer$get_mon_resources()
#' }
#' \preformatted{## Get resource's capacity
#' Simmer$get_capacity(name)
#' }\describe{
#'   \item{name}{the name of the resource}
#' }
#' \preformatted{## Get resource's queue size
#' Simmer$get_queue_size(name)
#' }\describe{
#'   \item{name}{the name of the resource}
#' }
#' \preformatted{## Get resource's server count
#' Simmer$get_server_count(name)
#' }\describe{
#'   \item{name}{the name of the resource}
#' }
#' \preformatted{## Get resource's queue count
#' Simmer$get_queue_count(name)
#' }\describe{
#'   \item{name}{the name of the resource}
#' }
#' @format NULL
#' @usage NULL
#' @examples
#' t0 <- Trajectory$new("my trajectory") $
#'   ## add an intake activity
#'   seize("nurse", 1) $
#'   timeout(function() rnorm(1, 15)) $
#'   release("nurse", 1) $
#'   ## add a consultation activity
#'   seize("doctor", 1) $
#'   timeout(function() rnorm(1, 20)) $
#'   release("doctor", 1) $
#'   ## add a planning activity
#'   seize("administration", 1) $
#'   timeout(function() rnorm(1, 5)) $
#'   release("administration", 1)
#' 
#' simmer <- Simmer$new("SuperDuperSim") $
#'   add_resource("nurse", 1) $
#'   add_resource("doctor", 2) $
#'   add_resource("administration", 1) $
#'   add_generator("patient", t0, function() rnorm(1, 10, 2))
#'   
#' simmer$run(until=80)
#' 
#' plot_resource_usage(simmer, "doctor")
#' @useDynLib simmer
#' @importFrom R6 R6Class
#' @importFrom Rcpp evalCpp
#' @export
Simmer <- R6Class("Simmer",
  public = list(
    name = NA,
    
    initialize = function(name="anonymous", verbose=FALSE) {
      self$name <- evaluate_value(name)
      private$sim_obj <- Simulator__new(name, evaluate_value(verbose))
      invisible(self)
    },
    
    reset = function() { 
      reset_(private$sim_obj) 
      invisible(self)
    },
    
    now = function() { now_(private$sim_obj) },
    
    peek = function() {
      ret <- peek_(private$sim_obj)
      if (ret >= 0) ret
      else Inf
    },
    
    step = function() {
      step_(private$sim_obj)
      invisible(self)
    },
    
    run = function(until=1000) {
      until <- evaluate_value(until)
      if(!is.finite(until)) until <- 1000
      
      run_(private$sim_obj, until)
      invisible(self)
    },
    
    add_resource = function(name, capacity=1, queue_size=Inf, mon=TRUE) {
      name <- evaluate_value(name)
      capacity <- evaluate_value(capacity)
      queue_size <- evaluate_value(queue_size)
      if (is.infinite(capacity)) capacity <- -1
      if (is.infinite(queue_size)) queue_size <- -1
      mon <- evaluate_value(mon)
      
      add_resource_(private$sim_obj, name, capacity, queue_size, mon)
      if (mon) private$mon_res <- c(private$mon_res, name)
      invisible(self)
    },
    
    add_generator = function(name_prefix, trajectory, dist, mon=TRUE) {
      if (!inherits(trajectory, "Trajectory"))
        stop("not a trajectory")
      if (!is.function(dist))
        stop(paste0(self$name, ": dist must be callable"))
      name_prefix <- evaluate_value(name_prefix)
      mon <- evaluate_value(mon)

      add_generator_(private$sim_obj, name_prefix, trajectory$get_head(), dist, mon)
      invisible(self)
    },
    
    get_mon_arrivals = function() { 
      as.data.frame(get_mon_arrivals_(private$sim_obj))
    },
    
    get_mon_resources = function() {
      do.call(rbind,
        lapply(1:length(private$mon_res), function(j) {
          monitor_data <- as.data.frame(
            get_mon_resource_(private$sim_obj, private$mon_res[[j]])
          )
          tryCatch({
            monitor_data$system <- monitor_data$server + monitor_data$queue
            monitor_data$resource <- private$mon_res[[j]]
          }, error = function(e) {
            monitor_data$system <<- numeric()
            monitor_data$resource <<- character()
          })
          monitor_data
        })
      )
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
    }
  ),
  
  private = list(
    sim_obj = NULL,
    mon_res = NULL
  )
)
