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
        private$sim_objs <- c(private$sim_objs, Simulator$new(i, verbose))
      invisible(self)
    },
    
    reset = function() { 
      for (sim in private$sim_objs) 
        sim$reset() 
    },
    
    run = function(until=1000) {
      until <- evaluate_value(until)
      if(!is.finite(until)) until <- 1000
      for (sim in private$sim_objs) 
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
    }
  ),
  
  private = list(
    sim_objs = NULL
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
      invisible(self)
    },
    
    reset = function() {
      private$now_ <- 0
      private$queue <- PriorityQueue$new()
      for (res in private$res)
        res$reset()
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
    }
  ),
  
  active = list(
    now = function() { private$now_ }
  ),
  
  private = list(
    now_ = NA,
    queue = NULL,
    gen = NULL,
    res = NULL
  )
)
