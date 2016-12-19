#' @importFrom R6 R6Class
#' @importFrom Rcpp evalCpp
Trajectory <- R6Class("trajectory",
  public = list(
    name = NA,

    initialize = function(name="anonymous", verbose=FALSE) {
      self$name <- evaluate_value(name)
      private$verbose <- evaluate_value(verbose)
      self
    },

    print = function(indent=0) {
      margin <- paste(rep(" ", indent), collapse = "")
      cat(paste0(margin, "trajectory: ", self$name, ", ",
                 private$n_activities, " activities\n"))
      ptr <- self$get_head()
      while (!identical(ptr, self$get_tail())) {
        activity_print_(ptr, indent)
        ptr <- activity_get_next_(ptr)
      }
      if (!is.null(ptr)) activity_print_(ptr, indent)
    },

    get_head = function() { private$ptrs[[1]] },

    get_tail = function() { private$ptrs[[length(private$ptrs)]] },

    get_n_activities = function() { private$n_activities },

    seize = function(resource, amount=1, id=0, continue=NULL, post.seize=NULL, reject=NULL) {
      resource <- evaluate_value(resource)
      amount <- evaluate_value(amount)
      id <- evaluate_value(id)
      trj <- list()
      mask <- 0
      if (!is.null(post.seize)) {
        if (!inherits(post.seize, "trajectory")) stop("not a trajectory")
        trj <- c(trj, post.seize)
        mask <- mask + 1
      }
      if (!is.null(reject)) {
        if (!inherits(reject, "trajectory")) stop("not a trajectory")
        trj <- c(trj, reject)
        mask <- mask + 2
      }
      if (length(continue) != length(trj))
        stop("the number of elements does not match")
      if (!length(continue)) continue <- TRUE

      if (is.na(resource)) {
        if (is.function(amount))
          private$add_activity(SeizeSelected__new_func(private$verbose, id, amount, needs_attrs(amount),
                                                       continue, trj, mask))
        else private$add_activity(SeizeSelected__new(private$verbose, id, amount, continue, trj, mask))
      } else {
        if (is.function(amount))
          private$add_activity(Seize__new_func(private$verbose, resource, amount, needs_attrs(amount),
                                               continue, trj, mask))
        else private$add_activity(Seize__new(private$verbose, resource, amount, continue, trj, mask))
      }
    },

    release = function(resource, amount=1, id=0) {
      resource <- evaluate_value(resource)
      amount <- evaluate_value(amount)
      id <- evaluate_value(id)
      if (is.na(resource)) {
        if (is.function(amount))
          private$add_activity(ReleaseSelected__new_func(private$verbose, id, amount, needs_attrs(amount)))
        else private$add_activity(ReleaseSelected__new(private$verbose, id, amount))
      } else {
        if (is.function(amount))
          private$add_activity(Release__new_func(private$verbose, resource, amount, needs_attrs(amount)))
        else private$add_activity(Release__new(private$verbose, resource, amount))
      }
    },

    set_capacity = function(resource, value, id=0) {
      resource <- evaluate_value(resource)
      value <- evaluate_value(value)
      id <- evaluate_value(id)
      if (is.na(resource)) {
        if (is.function(value))
          private$add_activity(SetCapacitySelected__new_func(private$verbose, id, value, needs_attrs(value)))
        else private$add_activity(SetCapacitySelected__new(private$verbose, id, value))
      } else {
        if (is.function(value))
          private$add_activity(SetCapacity__new_func(private$verbose, resource, value, needs_attrs(value)))
        else private$add_activity(SetCapacity__new(private$verbose, resource, value))
      }
    },

    set_queue_size = function(resource, value, id=0) {
      resource <- evaluate_value(resource)
      value <- evaluate_value(value)
      id <- evaluate_value(id)
      if (is.na(resource)) {
        if (is.function(value))
          private$add_activity(SetQueueSelected__new_func(private$verbose, id, value, needs_attrs(value)))
        else private$add_activity(SetQueueSelected__new(private$verbose, id, value))
      } else {
        if (is.function(value))
          private$add_activity(SetQueue__new_func(private$verbose, resource, value, needs_attrs(value)))
        else private$add_activity(SetQueue__new(private$verbose, resource, value))
      }
    },

    select = function(resources, policy=c("shortest-queue", "round-robin",
                                          "first-available", "random"), id=0) {
      resources <- evaluate_value(resources)
      policy <- match.arg(policy)
      id <- evaluate_value(id)
      if (is.function(resources))
        private$add_activity(Select__new_func(private$verbose, resources, needs_attrs(resources), id))
      else private$add_activity(Select__new(private$verbose, resources, policy, id))
    },

    timeout = function(task) {
      task <- evaluate_value(task)
      if (is.function(task))
        private$add_activity(Timeout__new_func(private$verbose, task, needs_attrs(task)))
      else private$add_activity(Timeout__new(private$verbose, task))
    },

    set_attribute = function(key, value, global=FALSE) {
      key <- as.character(key)
      value <- evaluate_value(value)
      global <- evaluate_value(global)
      if (is.function(value))
        private$add_activity(SetAttribute__new_func(private$verbose, key, value, needs_attrs(value), global))
      else private$add_activity(SetAttribute__new(private$verbose, key, value, global))
    },

    activate = function(generator) {
      generator <- evaluate_value(generator)
      if (is.function(generator))
        private$add_activity(Activate__new_func(private$verbose, generator, needs_attrs(generator)))
      else private$add_activity(Activate__new(private$verbose, generator))
    },

    deactivate = function(generator) {
      generator <- evaluate_value(generator)
      if (is.function(generator))
        private$add_activity(Deactivate__new_func(private$verbose, generator, needs_attrs(generator)))
      else private$add_activity(Deactivate__new(private$verbose, generator))
    },

    set_trajectory = function(generator, trajectory) {
      if (!inherits(trajectory, "trajectory"))
        stop("not a trajectory")
      generator <- evaluate_value(generator)
      if (is.function(generator))
        private$add_activity(SetTraj__new_func(private$verbose, generator,
                                               needs_attrs(generator), trajectory))
      else private$add_activity(SetTraj__new(private$verbose, generator, trajectory))
    },

    set_distribution = function(generator, distribution) {
      generator <- evaluate_value(generator)
      distribution <- make_resetable(distribution)
      if (is.function(generator))
        private$add_activity(SetDist__new_func(private$verbose, generator,
                                               needs_attrs(generator), distribution))
      else private$add_activity(SetDist__new(private$verbose, generator, distribution))
    },

    set_prioritization = function(values) {
      if (is.function(values))
        private$add_activity(SetPrior__new_func(private$verbose, values, needs_attrs(values)))
      else private$add_activity(SetPrior__new(private$verbose, values))
    },

    branch = function(option, continue, ...) {
      trj <- list(...)
      if (length(continue) != length(trj))
        stop("the number of elements does not match")
      for (i in trj) if (!inherits(i, "trajectory"))
        stop("not a trajectory")
      private$add_activity(Branch__new(private$verbose, option, needs_attrs(option), continue, trj))
    },

    rollback = function(amount, times=1, check) {
      amount <- evaluate_value(amount)
      times <- evaluate_value(times)
      if (is.infinite(times)) times <- -1
      if (missing(check))
        private$add_activity(Rollback__new(private$verbose, amount, times))
      else private$add_activity(Rollback__new_func(private$verbose, amount, check, needs_attrs(check)))
    },

    leave = function(prob) {
      prob <- evaluate_value(prob)
      if (is.function(prob))
        private$add_activity(Leave__new_func(private$verbose, prob, needs_attrs(prob)))
      else private$add_activity(Leave__new(private$verbose, prob))
    },

    renege_in = function(t, out=NULL) {
      t <- evaluate_value(t)
      traj <- list()
      if (!is.null(out)) {
        if (!inherits(out, "trajectory")) stop("not a trajectory")
        traj <- c(traj, out)
      }
      if (is.function(t))
        private$add_activity(RenegeIn__new_func(private$verbose, t, needs_attrs(t), traj))
      else private$add_activity(RenegeIn__new(private$verbose, t, traj))
    },

    renege_if = function(signal, out=NULL) {
      signal <- evaluate_value(signal)
      traj <- list()
      if (!is.null(out)) {
        if (!inherits(out, "trajectory")) stop("not a trajectory")
        traj <- c(traj, out)
      }
      if (is.function(signal))
        private$add_activity(RenegeIf__new_func(private$verbose, signal, needs_attrs(signal), traj))
      else private$add_activity(RenegeIf__new(private$verbose, signal, traj))
    },

    renege_abort = function() { private$add_activity(RenegeAbort__new(private$verbose)) },

    replicate = function(n, ...) {
      n <- evaluate_value(n)
      trj <- list(...)
      for (i in trj) if (!inherits(i, "trajectory"))
        stop("not a trajectory")
      if (is.function(n))
        private$add_activity(Clone__new_func(private$verbose, n, needs_attrs(n), trj))
      else private$add_activity(Clone__new(private$verbose, n, trj))
    },

    synchronize = function(wait=TRUE, mon_all=FALSE) {
      wait <- evaluate_value(wait)
      mon_all <- evaluate_value(mon_all)
      private$add_activity(Synchronize__new(private$verbose, wait, mon_all))
    },

    batch = function(n, timeout=0, permanent=FALSE, name="", rule=NULL) {
      n <- evaluate_value(n)
      timeout <- evaluate_value(timeout)
      permanent <- evaluate_value(permanent)
      name <- evaluate_value(name)
      if (is.function(rule))
        private$add_activity(Batch__new_func(private$verbose, n, timeout, permanent, name,
                                             rule, needs_attrs(rule)))
      else private$add_activity(Batch__new(private$verbose, n, timeout, permanent, name))
    },

    separate = function() { private$add_activity(Separate__new(private$verbose)) },

    send = function(signals, delay=0) {
      signals <- evaluate_value(signals)
      delay <- evaluate_value(delay)
      if (is.function(signals) && is.function(delay))
        private$add_activity(Send__new_func4(private$verbose, signals, delay,
                                             c(needs_attrs(signals), needs_attrs(delay))))
      else if (is.function(delay))
        private$add_activity(Send__new_func2(private$verbose, signals, delay, needs_attrs(delay)))
      else if (is.function(signals))
        private$add_activity(Send__new_func1(private$verbose, signals, delay, needs_attrs(signals)))
      else private$add_activity(Send__new(private$verbose, signals, delay))
    },

    trap = function(signals, handler=NULL, interruptible=TRUE) {
      signals <- evaluate_value(signals)
      interruptible <- evaluate_value(interruptible)
      traj <- list()
      if (!is.null(handler)) {
        if (!inherits(handler, "trajectory")) stop("not a trajectory")
        traj <- c(traj, handler)
      }
      if (is.function(signals))
        private$add_activity(Trap__new_func(private$verbose, signals, needs_attrs(signals),
                                            traj, interruptible))
      else private$add_activity(Trap__new(private$verbose, signals, traj, interruptible))
    },

    untrap = function(signals) {
      signals <- evaluate_value(signals)
      if (is.function(signals))
        private$add_activity(UnTrap__new_func(private$verbose, signals, needs_attrs(signals)))
      else private$add_activity(UnTrap__new(private$verbose, signals))
    },

    wait = function() { private$add_activity(Wait__new(private$verbose)) },

    log = function(message) {
      message <- evaluate_value(message)
      if (is.function(message))
        private$add_activity(Log__new_func(private$verbose, message, needs_attrs(message)))
      else private$add_activity(Log__new(private$verbose, message))
    },

    join = function(trajectory) {
      if (!inherits(trajectory, "trajectory"))
        stop("not a trajectory")
      new <- self$clone(deep = TRUE)
      trajectory <- trajectory$clone(deep = TRUE)
      if (!is.null(trajectory$get_head()) && !is.null(new$get_tail()))
          activity_chain_(new$get_tail(), trajectory$get_head())
      new$.__enclos_env__$private$ptrs <-
        c(new$.__enclos_env__$private$ptrs, trajectory$.__enclos_env__$private$ptrs)
      new$.__enclos_env__$private$n_activities <-
        new$.__enclos_env__$private$n_activities + trajectory$get_n_activities()
      new
    }
  ),

  private = list(
    verbose = FALSE,
    n_activities = 0,
    ptrs = NULL,

    add_activity = function(activity) {
      if (!is.null(private$ptrs))
        activity_chain_(self$get_tail(), activity)
      private$ptrs <- c(private$ptrs, activity)
      private$n_activities <- private$n_activities + activity_get_n_(activity)
      self
    },

    clone2 = function(){},
    copy = function(deep = FALSE) {
      new <- private$clone2(deep)
      new$.__enclos_env__$private$ptrs <- NULL
      if (!is.null(self$get_head())) {
        ptr <- self$get_head()
        new_ptr <- activity_clone_(ptr)
        new$.__enclos_env__$private$ptrs <- c(new$.__enclos_env__$private$ptrs, new_ptr)
        while (!identical(ptr, self$get_tail())) {
          ptr <- activity_get_next_(ptr)
          new_ptr <- activity_clone_(ptr)
          activity_chain_(new$get_tail(), new_ptr)
          new$.__enclos_env__$private$ptrs <- c(new$.__enclos_env__$private$ptrs, new_ptr)
        }
      }
      new
    }
  )
)
Trajectory$private_methods$clone2 <- Trajectory$public_methods$clone
Trajectory$public_methods$clone <- Trajectory$private_methods$copy
