require(R6)

#' Simmer
#'
#' Simulator object
#'
#' @field name simulator name
#' @format An \code{\link{R6Class}} generator object
#' @export
Simmer <- R6Class("Simmer",
  public = list(
    name = NA,
    
    initialize = function(name="anonymous", verbose=FALSE) {
      self$name <- name
      private$verbose <- verbose
      private$now_ <- 0
      private$queue <- PriorityQueue$new()
      private$gen <- list()
      private$res <- list()
    },
    
    reset = function() {
      private$now_ <- 0
      private$queue <- PriorityQueue$new()
      for (res in private$res)
        res$reset()
    },
    
    schedule = function(delay, event) {
      private$queue$push(self$now + delay, event)
    },
    
    run = function(until=1000, rep=1) {
      until <- evaluate_value(until)
      rep <- evaluate_value(rep)
      if(!is.finite(until)) until <- 1000
      if(!is.finite(rep)) rep <- 1
      
      while (rep > 0) {
        # Initialisation
        if (!private$queue$length()) {
          if (!length(private$gen))
            stop("no generators defined")
          for (gen in private$gen)
            gen$activate()
        }
      
        # Loop
        while (self$now < until) {
          entity <- self$queue$pop()
          private$now_ <- entity[[1]]
          entity[[2]]$activate()
        }
        
        self$reset()
        rep <- rep - 1
      }
    },
    
    add_resource = function() {
      
    },
    
    add_generator = function() {
      
    }
  ),
  
  active = list(
    now = function() { private$now_ }
  ),
  
  private = list(
    verbose = NA,
    now_ = NA,
    queue = NA,
    gen = NA,
    res = NA
  )
)

add_resource<-function(sim_obj, name, capacity, queue_size = Inf){
  if(is.infinite(queue_size)) queue_size <- -1
  for(sim_ptr in sim_obj@simulators) add_resource_(sim_ptr, name, capacity, queue_size)
  
  return(sim_obj)
}
