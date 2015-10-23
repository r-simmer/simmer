require(R6)

Entity <- R6Class("Entity",
  public = list(
    name = NA,
    
    initialize = function(sim, name) {
      if (!inherits(sim, "Simmer"))
        stop("not a simulator")
      private$sim <- sim
      self$name <- evaluate_value(name)
    }
  ),
  
  private = list(
    sim = NA
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
    
    seize = function(customer) {
      if (!inherits(customer, "Customer"))
        stop("not a customer")
      if (private$room_in_server()) {
        private$server <- c(private$server, customer)
        customer$activate()
      } else if (private$room_in_queue())
        private$queue <- c(private$queue, customer)
      else remove(customer)
    },
    
    release = function() {
      # departure
      customer <- private$server[[1]]
      private$server <- private$server[-1]
      customer$activate()
      # serve from the queue
      customer <- private$queue[[1]]
      private$queue <- private$queue[-1]
      if (!is.null(customer)) customer$activate()
    }
  ),
  
  private = list(
    server = NA,
    capacity = NA,
    queue = NA,
    queue_size = NA,
    
    room_in_server = function() { return(length(private$server) < private$capacity) },
    room_in_queue = function() { return(length(private$queue) < private$queue_size) }
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
    },
    
    activate = function() {
      
    }
  ),
  
  private = list(
    traj = NA,
    dist = NA
  )
)

Customer <- R6Class("Customer", inherit = Process,
  public = list(
    initialize = function(sim, name_prefix, first_event) {
      super$initialize(sim, name_prefix)
      if (!inherits(first_event, "Event"))
        stop("not an event")
      private$event <- first_event
    },
    
    activate = function() {
      current_event <- private$event
      private$event <- private$event$next_event
      current_event$run()
    }
  ),
  
  private = list(
    event = NULL
  )
)
