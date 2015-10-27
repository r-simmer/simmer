require(R6)

#' Simmer
#'
#' Simulation environment
#'
#' @field name environment name
#' @format An \code{\link{R6Class}} generator object
#' @examples
#' simmer <- Simmer$new("SuperDuperSim", rep=100, verbose=F) $
#'   add_resource("nurse", 1) $
#'   add_resource("doctor", 2) $
#'   add_resource("administration", 1) $
#'   add_generator("patient", t1, function() rnorm(1, 10, 2))
#' simmer$run(until=80)
#' @useDynLib simmer
#' @importFrom Rcpp evalCpp
#' @import R6
#' @export
Simmer <- R6Class("Simmer",
  public = list(
    name = NA,
    
    initialize = function(name="anonymous", rep=1, verbose=FALSE) {
      self$name <- evaluate_value(name)
      rep <- evaluate_value(rep)
      if(!is.finite(rep)) rep <- 1
      for (i in seq(rep))
        private$sim_objs <- c(private$sim_objs,
                              Simulator__new(i, evaluate_value(verbose)))
      invisible(self)
    },
    
    reset = function() { 
      for (sim in private$sim_objs) 
        reset_(sim) 
      invisible(self)
    },
    
    run = function(until=1000, parallel=0) {
      until <- evaluate_value(until)
      if(!is.finite(until)) until <- 1000
      
      # TODO parallelize in C++
      parallel <- evaluate_value(parallel)
      
      for (sim in private$sim_objs)
        run_(sim, until)
    },
    
    add_resource = function(name, capacity=1, queue_size=Inf, mon=T) {
      name <- evaluate_value(name)
      capacity <- evaluate_value(capacity)
      queue_size <- evaluate_value(queue_size)
      mon <- evaluate_value(mon)
      if (is.infinite(capacity)) capacity <- -1
      if (is.infinite(queue_size)) queue_size <- -1
      
      for (sim in private$sim_objs)
        add_resource_(sim, name, capacity, queue_size, mon)
      if (mon) private$mon_res <- c(private$mon_res, name)
      invisible(self)
    },
    
    add_generator = function(name_prefix, trajectory, dist, mon=T) {
      if (!inherits(trajectory, "Trajectory"))
        stop("not a trajectory")
      if (!is.function(dist))
        stop(paste0(self$name, ": dist must be callable"))
      name_prefix <- evaluate_value(name_prefix)
      mon <- evaluate_value(mon)
      
      for (sim in private$sim_objs)
        add_generator_(sim, name_prefix, trajectory$get_head(), dist, mon)
      invisible(self)
    },
    
    get_mon_arrivals = function() {
      do.call(rbind,
        lapply(1:length(private$sim_objs), function(i) {
          monitor_data <- as.data.frame(
            get_mon_arrivals_(private$sim_objs[[i]])
          )
          monitor_data$replication <- i
          monitor_data
        })
      )
    },
    
    get_mon_resources = function() {
      do.call(rbind,
        sapply(1:length(private$sim_objs), function(i) {
          lapply(1:length(private$mon_res), function(j, i) {
            monitor_data <- as.data.frame(
              get_mon_resource_(private$sim_objs[[i]], private$mon_res[[j]])
            )
            monitor_data$system <- monitor_data$server + monitor_data$queue
            monitor_data$resource <- private$mon_res[[j]]
            monitor_data$replication <- i
            monitor_data
          }, i=i)
        })
      )
    },
    
    get_res_capacity = function(name) { 
      get_res_capacity_(private$sim_objs[[1]], evaluate_value(name))
    },
    
    get_res_queue_size = function(name) {
      get_res_queue_size_(private$sim_objs[[1]], evaluate_value(name))
    }
  ),
  
  private = list(
    sim_objs = NULL,
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
