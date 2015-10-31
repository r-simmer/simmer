require(R6)

#' Simmer
#'
#' Simulation environment
#'
#' @field name environment name
#' @format An \code{\link{R6Class}} generator object
#' @examples
#' library(simmer)
#' 
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
#' @useDynLib simmer
#' @importFrom Rcpp evalCpp
#' @import R6
#' @export
Simmer <- R6Class("Simmer",
  public = list(
    name = NA,
    
    initialize = function(name="anonymous", verbose=FALSE) {
      self$name <- evaluate_value(name)
      private$sim_obj <- Simulator__new(name, verbose)
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
    
    add_resource = function(name, capacity=1, queue_size=Inf, mon=T) {
      name <- evaluate_value(name)
      capacity <- evaluate_value(capacity)
      queue_size <- evaluate_value(queue_size)
      if (is.infinite(capacity)) capacity <- -1
      if (is.infinite(queue_size)) queue_size <- -1
      
      add_resource_(private$sim_obj, name, capacity, queue_size, mon)
      if (mon) private$mon_res <- c(private$mon_res, name)
      invisible(self)
    },
    
    add_generator = function(name_prefix, trajectory, dist, mon=T) {
      if (!inherits(trajectory, "Trajectory"))
        stop("not a trajectory")
      if (!is.function(dist))
        stop(paste0(self$name, ": dist must be callable"))
      name_prefix <- evaluate_value(name_prefix)

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
    
    get_res_capacity = function(name) { 
      get_res_capacity_(private$sim_obj, evaluate_value(name))
    },
    
    get_res_queue_size = function(name) {
      get_res_queue_size_(private$sim_obj, evaluate_value(name))
    }
  ),
  
  private = list(
    sim_obj = NULL,
    mon_res = NULL
  )
)

evaluate_value<-function(value){
  tryCatch(
    {
      abs(parse(text=value))
    }, 
    error = function(err) value)
}
