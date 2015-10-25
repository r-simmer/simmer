require(R6)

Entity <- R6Class("Entity",
  public = list(
    name = NA,
    sim = NULL,
    
    initialize = function(sim, name) {
      self$sim <- sim
      self$name <- name
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
    initialize = function(sim, name, capacity, queue_size, mon) {
      super$initialize(sim, name)
      private$capacity <- capacity
      private$queue_size <- queue_size
      private$mon <- mon
      private$res_stats <- list(
        time = numeric(),
        server = numeric(),
        queue = numeric()
      )
    },
    
    reset = function() {
      private$server <- NULL
      private$server_count <- NULL
      private$queue <- NULL
      private$queue_count <- NULL
      private$res_stats <- list(
        time = numeric(),
        server = numeric(),
        queue = numeric()
      )
    },
    
    seize = function(arrival, amount) {
      if (private$mon) self$observe(arrival$sim$now)
      
      if (private$room_in_server(amount)) {
        private$server <- c(private$server, arrival)
        private$server_count <- c(private$server_count, amount)
        return(0) # serving now
      } else if (private$room_in_queue(amount)) {
        private$queue <- c(private$queue, arrival)
        private$queue_count <- c(private$queue_count, amount)
        return(1) # enqueued
      } else {
        arrival$leave()
        return(-1) # reject
      }
    },
    
    release = function(arrival, amount) {
      if (private$mon) self$observe(arrival$sim$now)
      
      # departure
      saved_arrival <- private$server[[1]]
      saved_amount <- private$server_count[[1]]
      private$server <- private$server[-1]
      private$server_count <- private$server_count[-1]
      if ((!identical(saved_arrival, arrival)) || (!identical(saved_amount, amount)))
        stop(paste0(self$name, ": inconsistent trajectory detected"))
      
      # serve from the queue
      if (length(private$queue_count)) {
        another_arrival <- private$queue[[1]]
        another_amount <- private$queue_count[[1]]
        if (!is.null(another_arrival)) {
          private$queue <- private$queue[-1]
          private$queue_count <- private$queue_count[-1]
          private$server <- c(private$server, another_arrival)
          private$server_count <- c(private$server_count, another_amount)
          another_arrival$sim$schedule(0, another_arrival)
        }
      }
      return(0)
    },
    
    observe = function(time) {
      private$res_stats[[1]] <- c(private$res_stats[[1]], time)
      private$res_stats[[2]] <- c(private$res_stats[[2]], sum(private$server_count))
      private$res_stats[[3]] <- c(private$res_stats[[3]], sum(private$queue_count))
    },
    
    get_observations = function() { private$res_stats },
    
    get_capacity = function() { private$capacity },
    
    get_queue_size = function() { private$queue_size }
  ),
  
  private = list(
    server = NULL,
    server_count = NULL,
    capacity = NA,
    queue = NULL,
    queue_count = NULL,
    queue_size = NA,
    mon = NA,
    res_stats = NA,
    
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
      private$traj <- trajectory
      private$dist <- dist
      private$count <- 0
    },
    
    activate = function() {
      delay <- private$dist()
      arrival <- Arrival$new(self$sim, 
                               paste0(self$name, private$count),
                               private$traj$get_head())
      self$sim$schedule(delay, self)
      self$sim$schedule(delay, arrival)
      private$count <- private$count + 1
    }
  ),
  
  private = list(
    count = NA,
    traj = NULL,
    dist = NULL
  )
)

Arrival <- R6Class("Arrival", inherit = Process,
  public = list(
    initialize = function(sim, name, first_event) {
      super$initialize(sim, name)
      private$event <- first_event
      private$activity <- 0
    },
    
    activate = function() {
      if (is.na(private$start))
        private$start <- self$sim$now
      
      current_event <- private$event
      if (is.null(current_event))
        self$leave()
      else {
        if (self$sim$verbose)
          cat(paste("rep:", self$sim$name, "|",
              "time:", self$sim$now, "|",
              "arrival:", self$name, "|",
              "event:", current_event$name, "\n"))
        private$event <- private$event$next_event
        private$activity <- private$activity + 
          current_event$run(self)
      }
    },
    
    leave = function() {
      self$sim$notify(self$name,
                      private$start,
                      self$sim$now,
                      private$activity, 
                      is.null(private$event))
      # destroy?
    }
  ),
  
  private = list(
    start = NA,
    activity = NA,
    event = NULL
  )
)
