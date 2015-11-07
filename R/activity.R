require(R6)

Activity <- R6Class("Activity",
  public = list(
    name = NA,
    n = 1,
    resource = NA,

    show = function(indent=0) {
      margin <- paste(rep(" ", indent), collapse="")
      cat(paste0(margin, "{ Activity: ", self$name, " | "))
      for (i in names(private)) {
        if (i != "ptr")
          if (is.function(private[[i]]))
            cat(i, ": function(), ", sep = "")
          else
            cat(i, ": ", private[[i]], ", ", sep = "")
      }
      cat("}\n")
    },
    
    run = function(parent) { stop("not implemented") }
  ),
  
  active = list(
    next_activity = function(activity) {
      if (!missing(activity)) 
        private$ptr <- activity
      else return(private$ptr)
    }
  ),
  
  private = list(
    ptr = NULL
  )
)

SeizeActivity <- R6Class("SeizeActivity", inherit = Activity,
  public = list(
    name = "Seize",
    
    initialize = function(resource, amount) {
      self$resource <- evaluate_value(resource)
      private$amount <- evaluate_value(amount)
    },
    
    run = function(parent) {
      return(seize_(self$resource, parent, private$amount))
    }
  ),
  
  private = list(
    amount = NA
  )
)

ReleaseActivity <- R6Class("ReleaseActivity", inherit = Activity,
  public = list(
    name = "Release",
    
    initialize = function(resource, amount) {
      self$resource <- evaluate_value(resource)
      private$amount <- evaluate_value(amount)
    },
    
    run = function(parent) {
      return(release_(self$resource, parent, private$amount))
    }
  ),
  
  private = list(
    amount = NA
  )
)

TimeoutActivity <- R6Class("TimeoutActivity", inherit = Activity,
  public = list(
    name = "Timeout",
    
    initialize = function(duration) {
      self$resource <- "None"
      if (!is.function(duration)) 
        stop(paste0(self$name, ": duration must be callable"))
      private$duration <- evaluate_value(duration)
    },
    
    run = function(parent) {
      ret <- private$duration()
      if (!is.numeric(ret)) {
        stop("non-numeric value")
        return(-1)
      }
      return(abs(ret))
    }
  ),
  
  private = list(
    duration = NULL
  )
)

BranchActivity <- R6Class("BranchActivity", inherit = Activity,
  public = list(
    name = "Branch",
   
    initialize = function(prob, merge, ...) {
      self$resource <- "None"
      self$n <- 0
      trj <- list(...)
      if (sum(prob) != 1)
        stop("prob must sum 1")
      if ((length(prob) != length(merge)) || (length(prob) != length(trj)))
        stop("the number of elements does not match")
      
      private$prob <- prob
      private$merge <- merge
      private$trj <- trj
      for (i in 1:length(trj)) {
        if (!inherits(trj[[i]], "Trajectory"))
          stop("not a trajectory")
        private$path <- c(private$path, trj[[i]]$get_head())
        if (merge[[i]]) {
          aux <- trj[[i]]$get_tail()
          aux$next_activity <- self
        }
        self$n <- self$n + trj[[i]]$get_n_activities()
      }
    },
    
    show = function(indent=0) {
      margin <- paste(rep(" ", indent), collapse="")
      for (i in 1:length(private$trj)) {
        cat(paste0(margin, "Branch | prob: ", private$prob[[i]],
                   " | merge: ", private$merge[[i]]), "\n")
        private$trj[[i]]$show(indent+2)
      }
    },
   
    run = function(parent) {
      if (parent %in% private$pending)
        private$pending <- private$pending[-match(parent, private$pending)]
      else {
        i <- sample(1:length(private$path), 1, prob=private$prob)
        private$selected <- private$path[[i]]
        if (private$merge[[i]])
          private$pending <- c(private$pending, parent)
      }
      return(0)
    }
  ),
  
  active = list(
    next_activity = function(activity) {
      if (!missing(activity)) 
        private$ptr <- activity
      else {
        if (!is.null(private$selected)) {
          # Little trick to return 2 times the same pointer
          if (private$flag) {
            aux <- private$selected
            private$selected <- NULL
            private$flag <- FALSE
            return(aux)
          } else {
            private$flag <- TRUE
            return(private$selected)
          }
        } else return(private$ptr)
      }
    }
  ),
 
  private = list(
    prob = NA,
    merge = NA,
    trj = NA,
    path = NULL,
    selected = NULL,
    flag = FALSE,
    pending = NULL
  )
)
