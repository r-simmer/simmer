require(R6)

Entity <- R6Class("Entity",
  public = list(
    name = NA,
    sim = NULL,
    
    initialize = function(sim, name) {
      if (!inherits(sim, "Simulator"))
        stop("not a simulator")
      self$sim <- sim
      self$name <- evaluate_value(name)
    }
  )
)

Process <- R6Class("Process", inherit = Entity,
  public = list(
    activate = function() { stop("not implemented") }
  )
)

Resource <- R6Class("Resource", inherit = Entity,
  public = list(
    initialize = function(sim, name, capacity=1, queue_size=Inf) {
      super$initialize(sim, name)
      private$capacity <- evaluate_value(capacity)
      private$queue_size <- evaluate_value(queue_size)
    },
    
    reset = function() {
      private$server <- NULL
      private$server_count <- NULL
      private$queue <- NULL
      private$queue_count <- NULL
    },
    
    seize = function(customer, amount) {
      if (!inherits(customer, "Customer"))
        stop("not a customer")
      
      if (private$room_in_server(amount)) {
        private$server <- c(private$server, customer)
        private$server_count <- c(private$server_count, amount)
        return(0) # serving now
      } else if (private$room_in_queue(amount)) {
        private$queue <- c(private$queue, customer)
        private$queue_count <- c(private$queue_count, amount)
        return(1) # enqueued
      } else {
        customer$leave()
        return(-1) # reject
      }
    },
    
    release = function(customer, amount) {
      # departure
      saved_customer <- private$server[[1]]
      saved_amount <- private$server_count[[1]]
      private$server <- private$server[-1]
      private$server_count <- private$server_count[-1]
      if ((!identical(saved_customer, customer)) || (!identical(saved_amount, amount)))
        stop(paste0(self$name, ": inconsistent trajectory detected"))
      
      # serve from the queue
      if (length(private$queue_count)) {
        another_customer <- private$queue[[1]]
        another_amount <- private$queue_count[[1]]
        if (!is.null(another_customer)) {
          private$queue <- private$queue[-1]
          private$queue_count <- private$queue_count[-1]
          private$server <- c(private$server, another_customer)
          private$server_count <- c(private$server_count, another_amount)
          another_customer$sim$schedule(0, another_customer)
        }
      }
      return(0)
    }
  ),
  
  private = list(
    server = NULL,
    server_count = NULL,
    capacity = NA,
    queue = NULL,
    queue_count = NULL,
    queue_size = NA,
    
    room_in_server = function(amount) {
      return(sum(private$server_count) + amount <= private$capacity)
    },
    
    room_in_queue = function(amount) {
      return(sum(private$queue_count) + amount <= private$queue_size)
    }
  )
)

Generator <- R6Class("Generator", inherit = Process,
  public = list(
    initialize = function(sim, name_prefix, trajectory, dist) {
      super$initialize(sim, name_prefix)
      if (!inherits(trajectory, "Trajectory"))
        stop("not a trajectory")
      private$traj <- trajectory
      if (!is.function(dist))
        stop(paste0(self$name, ": dist must be callable"))
      private$dist <- dist
      private$count <- 0
    },
    
    activate = function() {
      delay <- private$dist()
      customer <- Customer$new(self$sim, 
                               paste0(self$name, private$count),
                               private$traj$get_head())
      self$sim$schedule(delay, self)
      self$sim$schedule(delay, customer)
      private$count <- private$count + 1
    }
  ),
  
  private = list(
    count = NA,
    traj = NULL,
    dist = NULL
  )
)

Customer <- R6Class("Customer", inherit = Process,
  public = list(
    initialize = function(sim, name, first_event) {
      super$initialize(sim, name)
      if (!inherits(first_event, "Event"))
        stop("not an event")
      private$event <- first_event
      private$start <- sim$now
    },
    
    activate = function() {
      current_event <- private$event
      if (is.null(current_event)) 
        self$leave()
      else {
        if (self$sim$verbose)
          cat(paste("rep:", self$sim$name, "|",
              "time:", self$sim$now, "|",
              "customer:", self$name, "|",
              "event:", current_event$name, "\n"))
        private$event <- private$event$next_event
        current_event$run(self)
      }
    },
    
    leave = function() {
      # ???
    }
  ),
  
  private = list(
    start = NA,
    event = NULL
  )
)
