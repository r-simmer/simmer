require(R6)
require(doParallel)

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
    
    initialize = function(name="anonymous", rep=1, parallel=0, verbose=FALSE) {
      self$name <- evaluate_value(name)
      rep <- evaluate_value(rep)
      if(!is.finite(rep)) rep <- 1
      for (i in seq(rep))
        private$sim_objs <- c(private$sim_objs, Simulator$new(i, verbose))
      private$cluster <- evaluate_value(parallel)
      invisible(self)
    },
    
    reset = function() { 
      for (sim in private$sim_objs) 
        sim$reset() 
    },
    
    run = function(until=1000) {
      until <- evaluate_value(until)
      if(!is.finite(until)) until <- 1000
      
      if (private$cluster) {
        cl <- makeCluster(private$cluster, outfile="")
        registerDoParallel(cl)
        private$sim_objs <- 
          foreach (sim=iter(private$sim_objs)) %dopar%
            sim$run(until)
        stopCluster(cl)
      } else for (sim in private$sim_objs)
        sim$run(until)
    },
    
    add_resource = function(name, capacity=1, queue_size=Inf) {
      for (sim in private$sim_objs) 
        sim$add_resource(name, capacity, queue_size)
      invisible(self)
    },
    
    add_generator = function(name_prefix, trajectory, dist) {
      for (sim in private$sim_objs)
        sim$add_generator(name_prefix, trajectory, dist)
      invisible(self)
    },
    
    get_mon_customers = function() {
      do.call(rbind,
        lapply(1:length(private$sim_objs), function(i) {
          monitor_data <- as.data.frame(
            private$sim_objs[[i]]$get_mon_customers()
          )
          monitor_data$replication <- i
          monitor_data
        })
      )
    }
  ),
  
  private = list(
    sim_objs = NULL,
    cluster = NA
  )
)

Simulator <- R6Class("Simulator",
  public = list(
    name = NA,
    verbose = NA,
    
    initialize = function(name, verbose) {
      self$name <- evaluate_value(name)
      self$verbose <- evaluate_value(verbose)
      private$now_ <- 0
      private$queue <- PriorityQueue$new()
      private$gen <- list()
      private$res <- list()
      private$customer_stats <- list(
        name = character(),
        system_time = numeric(),
        activity_time = numeric(),
        finished = numeric()
      )
      invisible(self)
    },
    
    reset = function() {
      private$now_ <- 0
      private$queue <- PriorityQueue$new()
      for (res in private$res)
        res$reset()
      private$customer_stats <- list(
        name = character(),
        system_time = numeric(),
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
    
    add_resource = function(name, capacity, queue_size) {
      res <- Resource$new(self, name, capacity, queue_size)
      private$res[[name]] <- res
      invisible(self)
    },
    
    get_resource = function(name) { return(private$res[[name]]) },
    
    add_generator = function(name_prefix, trajectory, dist) {
      gen <- Generator$new(self, name_prefix, trajectory, dist)
      private$gen <- c(private$gen, gen)
      invisible(self)
    },
    
    notify = function(customer_name, system_time, activity_time, finished) {
      private$customer_stats[[1]] <- c(private$customer_stats[[1]], customer_name)
      private$customer_stats[[2]] <- c(private$customer_stats[[2]], system_time)
      private$customer_stats[[3]] <- c(private$customer_stats[[3]], activity_time)
      private$customer_stats[[4]] <- c(private$customer_stats[[4]], finished)
    },
    
    get_mon_customers = function() { private$customer_stats }
  ),
  
  active = list(
    now = function() { private$now_ }
  ),
  
  private = list(
    now_ = NA,
    queue = NULL,
    gen = NULL,
    res = NULL,
    customer_stats = NA
  )
)
