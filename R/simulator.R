require(R6)

#' Simmer.Env
#'
#' Simulation environment
#'
#' @field name environment name
#' @format An \code{\link{R6Class}} generator object
#' @useDynLib simmer
#' @importFrom Rcpp evalCpp
#' @import R6
#' @export
Simmer.Env <- R6Class("Simmer",
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
    
    step = function() { step_(private$sim_obj) },
    
    run = function(until=1000) {
      until <- evaluate_value(until)
      if(!is.finite(until)) until <- 1000
      
      run_(private$sim_obj, until)
    },
    
    resource = function(name, capacity=1, queue_size=Inf, mon=T) {
      name <- evaluate_value(name)
      capacity <- evaluate_value(capacity)
      queue_size <- evaluate_value(queue_size)
      mon <- evaluate_value(mon)
      if (is.infinite(capacity)) capacity <- -1
      if (is.infinite(queue_size)) queue_size <- -1
      
      resource_(private$sim_obj, name, capacity, queue_size, mon)
      if (mon) private$mon_res <- c(private$mon_res, name)
      invisible(self)
    },
    
    process = function(func) {
      if (!is.function(func))
        stop(paste0(self$name, ": func must be callable"))
      
      process_(private$sim_obj, func)
      invisible(self)
    },
    
    timeout = function(delay) {
      timeout_(private$sim_obj, evaluate_value(delay))
    },
    
    request = function(name, amount) {
      name <- evaluate_value(name)
      amount <- evaluate_value(amount)
      request_(private$sim_obj, name, amount)
    },
    
    get_mon_arrivals = function() {
      monitor_data <- as.data.frame(
        get_mon_arrivals_(private$sim_obj)
      )
      monitor_data
    },
    
    get_mon_resources = function() {
      do.call(rbind,
        lapply(1:length(private$mon_res), function(i) {
          monitor_data <- as.data.frame(
            get_mon_resource_(private$sim_obj, private$mon_res[[i]])
          )
          monitor_data$system <- monitor_data$server + monitor_data$queue
          monitor_data$resource <- private$mon_res[[i]]
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
