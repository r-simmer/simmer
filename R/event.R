require(R6)

Event <- R6Class("Event",
  public = list(
    name = NA,

    show = function() {
      cat(paste0("{ Event: ", self$name, " | "))
      for (i in names(private)) {
        if (i != "prob" && i != "ptr") {
          if (is.function(private[[i]]))
            cat(i, ": function(), ", sep = "")
          else
            cat(i, ": ", private[[i]], ", ", sep = "")
        }
      }
      cat("}")
    },
    
    run = function(parent) { stop("not implemented") }
  ),
  
  active = list(
    next_event = function(ev) {
      if (missing(ev)) {
        if (is.null(private$ptr)) return(private$ptr)
        else return(sample(private$ptr, 1, prob=private$prob)[[1]])
      } else {
        private$ptr <- c(private$ptr, ev[1])
        private$prob <- c(private$prob, ev[2])
      }
    }
  ),
  
  private = list(
    ptr = NULL,
    prob = NULL
  )
)

SeizeEvent <- R6Class("SeizeEvent", inherit = Event,
  public = list(
    name = "Seize",
    
    initialize = function(resource, amount) {
      private$resource <- evaluate_value(resource)
      private$amount <- evaluate_value(amount)
    },
    
    run = function(parent) {
      res <- parent$sim$get_resource(private$resource)
      if (!res$seize(parent, private$amount))
        parent$sim$schedule(0, parent)
      return(0)
    }
  ),
  
  private = list(
    resource = NA,
    amount = NA
  )
)

ReleaseEvent <- R6Class("ReleaseEvent", inherit = Event,
  public = list(
    name = "Release",
    
    initialize = function(resource, amount) {
      private$resource <- evaluate_value(resource)
      private$amount <- evaluate_value(amount)
    },
    
    run = function(parent) {
      res <- parent$sim$get_resource(private$resource)
      if (!res$release(parent, private$amount))
        parent$sim$schedule(0, parent)
      return(0)
    }
  ),
  
  private = list(
    resource = NA,
    amount = NA
  )
)

TimeoutEvent <- R6Class("TimeoutEvent", inherit = Event,
  public = list(
    name = "Timeout",
    
    initialize = function(duration) {
      if (!is.function(duration)) 
        stop(paste0(self$name, ": duration must be callable"))
      private$duration <- evaluate_value(duration)
    },
    
    run = function(parent) {
      delay <- private$duration()
      parent$sim$schedule(delay, parent)
      return(delay)
    }
  ),
  
  private = list(
    duration = NULL
  )
)
