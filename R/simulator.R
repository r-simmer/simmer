require(R6)

#' Simmer
#'
#' Simulation environment
#'
#' @field name environment name
#' @format An \code{\link{R6Class}} generator object
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
                              Simulator$new(i, evaluate_value(verbose)))
      invisible(self)
    },
    
    reset = function() { 
      for (sim in private$sim_objs) 
        sim$reset() 
      invisible(self)
    },
    
    run = function(until=1000, parallel=0) {
      until <- evaluate_value(until)
      if(!is.finite(until)) until <- 1000
      parallel <- evaluate_value(parallel)
      
      if (parallel) {
        require(doParallel)
        cl <- makeCluster(parallel, outfile="")
        registerDoParallel(cl)
        private$sim_objs <- 
          foreach (sim=iter(private$sim_objs)) %dopar%
            sim$run(until)
        stopCluster(cl)
      } else for (sim in private$sim_objs)
        sim$run(until)
    },
    
    add_resource = function(name, capacity=1, queue_size=Inf, mon=T) {
      for (sim in private$sim_objs) {
        name <- evaluate_value(name)
        mon <- evaluate_value(mon)
        sim$add_resource(name,
                         evaluate_value(capacity), 
                         evaluate_value(queue_size), 
                         mon
        )
      }
      if (mon) private$mon_res <- c(private$mon_res, name)
      invisible(self)
    },
    
    add_generator = function(name_prefix, trajectory, dist) {
      if (!inherits(trajectory, "Trajectory"))
        stop("not a trajectory")
      if (!is.function(dist))
        stop(paste0(self$name, ": dist must be callable"))
      
      for (sim in private$sim_objs)
        sim$add_generator(evaluate_value(name_prefix),
                          trajectory, 
                          dist
        )
      invisible(self)
    },
    
    get_mon_arrivals = function() {
      do.call(rbind,
        lapply(1:length(private$sim_objs), function(i) {
          monitor_data <- as.data.frame(
            private$sim_objs[[i]]$get_mon_arrivals()
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
              private$sim_objs[[i]]$get_mon_resources(private$mon_res[[j]])
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
      private$sim_objs[[1]]$get_resource(name)$get_capacity()
    },
    
    get_res_queue_size = function(name) {
      private$sim_objs[[1]]$get_resource(name)$get_queue_size()
    }
  ),
  
  private = list(
    sim_objs = NULL,
    mon_res = NULL
  )
)

Simulator <- R6Class("Simulator",
  public = list(
    name = NA,
    verbose = NA,
    
    initialize = function(name, verbose) {
      self$name <- name
      self$verbose <- verbose
      private$now_ <- 0
      private$queue <- PriorityQueue$new()
      private$gen <- list()
      private$res <- list()
      private$arrival_stats <- list(
        name = character(),
        start_time = numeric(),
        end_time = numeric(),
        activity_time = numeric(),
        finished = numeric()
      )
    },
    
    reset = function() {
      private$now_ <- 0
      private$queue <- PriorityQueue$new()
      for (res in private$res)
        res$reset()
      private$arrival_stats <- list(
        name = character(),
        start_time = numeric(),
        end_time = numeric(),
        activity_time = numeric(),
        finished = numeric()
      )
    },
    
    schedule = function(delay, entity) {
      private$queue$push(self$now + delay, entity)
    },
    
    run = function(until) {
      # Initialisation
      if (!private$queue$length()) {
        if (!length(private$gen))
          stop("no generators defined")
        for (gen in private$gen)
          gen$activate()
      }
    
      # Loop
      while (self$now < until) {
        entity <- private$queue$pop()
        private$now_ <- entity[[1]]
        entity[[2]]$activate()
      }
      return(self)
    },
    
    add_resource = function(name, capacity, queue_size, mon) {
      res <- Resource$new(self, name, capacity, queue_size, mon)
      private$res[[name]] <- res
    },
    
    get_resource = function(name) { private$res[[name]] },
    
    add_generator = function(name_prefix, trajectory, dist) {
      gen <- Generator$new(self, name_prefix, trajectory, dist)
      private$gen <- c(private$gen, gen)
    },
    
    notify = function(arrival_name, start_time, end_time, activity_time, finished) {
      private$arrival_stats[[1]] <- c(private$arrival_stats[[1]], arrival_name)
      private$arrival_stats[[2]] <- c(private$arrival_stats[[2]], start_time)
      private$arrival_stats[[3]] <- c(private$arrival_stats[[3]], end_time)
      private$arrival_stats[[4]] <- c(private$arrival_stats[[4]], activity_time)
      private$arrival_stats[[5]] <- c(private$arrival_stats[[5]], finished)
    },
    
    get_mon_arrivals = function() { private$arrival_stats },
    
    get_mon_resources = function(name) { private$res[[name]]$get_observations() }
  ),
  
  active = list(
    now = function() { private$now_ }
  ),
  
  private = list(
    now_ = NA,
    queue = NULL,
    gen = NULL,
    res = NULL,
    arrival_stats = NA
  )
)
