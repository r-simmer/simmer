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
    
    run = function() { stop("not implemented") }
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
    }
  ),
  
  private = list(
    duration = NA
  )
)
