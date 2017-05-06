Trajectory <- R6Class("trajectory",
  public = list(
    name = NA,
    verbose = NA,

    initialize = function(name="anonymous", verbose=FALSE) {
      self$name <- evaluate_value(name)
      self$verbose <- evaluate_value(verbose)
      self
    },

    print = function(indent=0, verbose=self$verbose) {
      margin <- paste(rep(" ", indent), collapse = "")
      cat(paste0(margin, "trajectory: ", self$name, ", ",
                 private$n_activities, " activities\n"))
      lapply(private$ptrs, function(i) activity_print_(i, indent, verbose))
      invisible()
    },

    subset = function(i, double=FALSE) {
      parts <- private$get_parts(i, double)
      new <- private$clone2(deep = TRUE)
      ptrs <- NULL
      names <- NULL
      n_activities <- 0
      if (length(parts)) {
        ptrs <- sapply(parts, function(i) {
          new_ptr <- activity_clone_(private$ptrs[[i]])
          n_activities <<- n_activities + activity_get_n_(new_ptr)
          new_ptr
        })
        mapply(function(i, j) activity_chain_(i, j), ptrs[-length(ptrs)], ptrs[-1])
        names <- private$names[parts]
      }
      new$.__enclos_env__$private$ptrs <- ptrs
      new$.__enclos_env__$private$names <- names
      new$.__enclos_env__$private$n_activities <- n_activities
      new
    },

    split = function() { lapply(seq_len(length(self)), self$subset) },

    replace = function(i, value, double=FALSE) {
      stopifnot(inherits(value, "trajectory"))
      if (!length(self)) self
      else {
        parts <- private$get_parts(i, double)
        verbose <- self$verbose
        new <- self$split()
        new[parts] <- value$split()
        new <- join(new)
        new$verbose <- verbose
        new
      }
    },

    join = function(traj) {
      stopifnot(inherits(traj, "trajectory"))
      new <- self$clone()
      traj <- traj$clone()
      if (!is.null(traj$head()) && !is.null(new$tail()))
        activity_chain_(new$tail(), traj$head())
      new$.__enclos_env__$private$ptrs <-
        c(new$.__enclos_env__$private$ptrs, traj$.__enclos_env__$private$ptrs)
      new$.__enclos_env__$private$names <-
        c(new$.__enclos_env__$private$names, traj$.__enclos_env__$private$names)
      new$.__enclos_env__$private$n_activities <-
        new$.__enclos_env__$private$n_activities + traj$get_n_activities()
      new
    },

    rep = function(times=1, length.out=NA, each=1) {
      join(rep(self$split(), times, length.out, each))
    },

    head = function() { private$ptrs[[1]] },

    tail = function() { private$ptrs[[length(self)]] },

    length = function() { length(private$ptrs) },

    get_n_activities = function() { private$n_activities },

    seize = function(resource, amount=1, id=0, continue=NULL, post.seize=NULL, reject=NULL) {
      stopifnot(length(continue) == length(c(post.seize, reject)))
      stopifnot(all(sapply(c(post.seize, reject), inherits, what = "trajectory")))
      resource <- evaluate_value(resource)
      amount <- evaluate_value(amount)
      id <- evaluate_value(id)
      if (!length(continue)) continue <- TRUE
      trj <- as.list(c(post.seize[], reject[]))
      mask <- sum(c(1, 2) * !sapply(list(post.seize, reject), is.null))
      switch(
        binarise(is.na(resource), is.function(amount)),
        private$add_activity(Seize__new(resource, amount, continue, trj, mask)),
        private$add_activity(SeizeSelected__new(id, amount, continue, trj, mask)),
        private$add_activity(Seize__new_func(resource, amount, needs_attrs(amount),
                                            continue, trj, mask)),
        private$add_activity(SeizeSelected__new_func(id, amount, needs_attrs(amount),
                                                     continue, trj, mask))
      )
    },

    release = function(resource, amount=1, id=0) {
      resource <- evaluate_value(resource)
      amount <- evaluate_value(amount)
      id <- evaluate_value(id)
      switch(
        binarise(is.na(resource), is.function(amount)),
        private$add_activity(Release__new(resource, amount)),
        private$add_activity(ReleaseSelected__new(id, amount)),
        private$add_activity(Release__new_func(resource, amount, needs_attrs(amount))),
        private$add_activity(ReleaseSelected__new_func(id, amount, needs_attrs(amount)))
      )
    },

    set_capacity = function(resource, value, id=0) {
      resource <- evaluate_value(resource)
      value <- evaluate_value(value)
      id <- evaluate_value(id)
      switch(
        binarise(is.na(resource), is.function(value)),
        private$add_activity(SetCapacity__new(resource, value)),
        private$add_activity(SetCapacitySelected__new(id, value)),
        private$add_activity(SetCapacity__new_func(resource, value, needs_attrs(value))),
        private$add_activity(SetCapacitySelected__new_func(id, value, needs_attrs(value)))
      )
    },

    set_queue_size = function(resource, value, id=0) {
      resource <- evaluate_value(resource)
      value <- evaluate_value(value)
      id <- evaluate_value(id)
      switch(
        binarise(is.na(resource), is.function(value)),
        private$add_activity(SetQueue__new(resource, value)),
        private$add_activity(SetQueueSelected__new(id, value)),
        private$add_activity(SetQueue__new_func(resource, value, needs_attrs(value))),
        private$add_activity(SetQueueSelected__new_func(id, value, needs_attrs(value)))
      )
    },

    select = function(resources, policy=c("shortest-queue", "round-robin",
                                          "first-available", "random"), id=0) {
      resources <- evaluate_value(resources)
      policy <- match.arg(policy)
      id <- evaluate_value(id)
      switch(
        binarise(is.function(resources)),
        private$add_activity(Select__new(resources, policy, id)),
        private$add_activity(Select__new_func(resources, needs_attrs(resources), policy, id))
      )
    },

    timeout = function(task) {
      task <- evaluate_value(task)
      switch(
        binarise(is.function(task)),
        private$add_activity(Timeout__new(task)),
        private$add_activity(Timeout__new_func(task, needs_attrs(task)))
      )
    },

    set_attribute = function(key, value, global=FALSE) {
      key <- as.character(key)
      value <- evaluate_value(value)
      global <- evaluate_value(global)
      switch(
        binarise(is.function(value)),
        private$add_activity(SetAttribute__new(key, value, global)),
        private$add_activity(SetAttribute__new_func(key, value, needs_attrs(value), global))
      )
    },

    activate = function(generator) {
      generator <- evaluate_value(generator)
      switch(
        binarise(is.function(generator)),
        private$add_activity(Activate__new(generator)),
        private$add_activity(Activate__new_func(generator, needs_attrs(generator)))
      )
    },

    deactivate = function(generator) {
      generator <- evaluate_value(generator)
      switch(
        binarise(is.function(generator)),
        private$add_activity(Deactivate__new(generator)),
        private$add_activity(Deactivate__new_func(generator, needs_attrs(generator)))
      )
    },

    set_trajectory = function(generator, trajectory) {
      stopifnot(inherits(trajectory, "trajectory"))
      generator <- evaluate_value(generator)
      switch(
        binarise(is.function(generator)),
        private$add_activity(SetTraj__new(generator, trajectory[])),
        private$add_activity(SetTraj__new_func(generator, needs_attrs(generator), trajectory[]))
      )
    },

    set_distribution = function(generator, distribution) {
      generator <- evaluate_value(generator)
      distribution <- make_resetable(distribution)
      switch(
        binarise(is.function(generator)),
        private$add_activity(SetDist__new(generator, distribution)),
        private$add_activity(SetDist__new_func(generator, needs_attrs(generator), distribution))
      )
    },

    set_prioritization = function(values) {
      switch(
        binarise(is.function(values)),
        private$add_activity(SetPrior__new(values)),
        private$add_activity(SetPrior__new_func(values, needs_attrs(values)))
      )
    },

    branch = function(option, continue, ...) {
      stopifnot(length(continue) == length(c(...)))
      stopifnot(all(sapply(c(...), inherits, what = "trajectory")))
      traj <- sapply(c(...), `[`)
      private$add_activity(Branch__new(option, needs_attrs(option), continue, traj))
    },

    rollback = function(amount, times=Inf, check) {
      amount <- evaluate_value(amount)
      times <- evaluate_value(times)
      if (is.infinite(times)) times <- -1
      switch(
        binarise(!missing(check)),
        private$add_activity(Rollback__new(amount, times)),
        private$add_activity(Rollback__new_func(amount, check, needs_attrs(check)))
      )
    },

    leave = function(prob) {
      prob <- evaluate_value(prob)
      switch(
        binarise(is.function(prob)),
        private$add_activity(Leave__new(prob)),
        private$add_activity(Leave__new_func(prob, needs_attrs(prob)))
      )
    },

    renege_in = function(t, out=NULL) {
      stopifnot(is.null(out) || inherits(out, "trajectory"))
      t <- evaluate_value(t)
      traj <- as.list(c(out[]))
      switch(
        binarise(is.function(t)),
        private$add_activity(RenegeIn__new(t, traj)),
        private$add_activity(RenegeIn__new_func(t, needs_attrs(t), traj))
      )
    },

    renege_if = function(signal, out=NULL) {
      stopifnot(is.null(out) || inherits(out, "trajectory"))
      signal <- evaluate_value(signal)
      traj <- as.list(c(out[]))
      switch(
        binarise(is.function(signal)),
        private$add_activity(RenegeIf__new(signal, traj)),
        private$add_activity(RenegeIf__new_func(signal, needs_attrs(signal), traj))
      )
    },

    renege_abort = function() { private$add_activity(RenegeAbort__new()) },

    replicate = function(n, ...) {
      stopifnot(all(sapply(c(...), inherits, what = "trajectory")))
      n <- evaluate_value(n)
      trj <- sapply(c(...), `[`)
      switch(
        binarise(is.function(n)),
        private$add_activity(Clone__new(n, trj)),
        private$add_activity(Clone__new_func(n, needs_attrs(n), trj))
      )
    },

    synchronize = function(wait=TRUE, mon_all=FALSE) {
      wait <- evaluate_value(wait)
      mon_all <- evaluate_value(mon_all)
      private$add_activity(Synchronize__new(wait, mon_all))
    },

    batch = function(n, timeout=0, permanent=FALSE, name="", rule=NULL) {
      n <- evaluate_value(n)
      if (is.infinite(n)) n <- -1
      timeout <- evaluate_value(timeout)
      permanent <- evaluate_value(permanent)
      name <- evaluate_value(name)
      switch(
        binarise(is.function(timeout), is.function(rule)),
        private$add_activity(Batch__new(n, timeout, permanent, name)),
        private$add_activity(Batch__new_func1(n, timeout, permanent, name,
                                              needs_attrs(timeout))),
        private$add_activity(Batch__new_func2(n, timeout, permanent, name, rule,
                                              needs_attrs(rule))),
        private$add_activity(Batch__new_func4(n, timeout, permanent, name, rule,
                                              c(needs_attrs(timeout), needs_attrs(rule))))
      )
    },

    separate = function() { private$add_activity(Separate__new()) },

    send = function(signals, delay=0) {
      signals <- evaluate_value(signals)
      delay <- evaluate_value(delay)
      switch(
        binarise(is.function(signals), is.function(delay)),
        private$add_activity(Send__new(signals, delay)),
        private$add_activity(Send__new_func1(signals, delay, needs_attrs(signals))),
        private$add_activity(Send__new_func2(signals, delay, needs_attrs(delay))),
        private$add_activity(Send__new_func4(signals, delay,
                                             c(needs_attrs(signals), needs_attrs(delay))))
      )
    },

    trap = function(signals, handler=NULL, interruptible=TRUE) {
      stopifnot(is.null(handler) || inherits(handler, "trajectory"))
      signals <- evaluate_value(signals)
      interruptible <- evaluate_value(interruptible)
      traj <- as.list(c(handler[]))
      switch(
        binarise(is.function(signals)),
        private$add_activity(Trap__new(signals, traj, interruptible)),
        private$add_activity(Trap__new_func(signals, needs_attrs(signals), traj, interruptible))
      )
    },

    untrap = function(signals) {
      signals <- evaluate_value(signals)
      switch(
        binarise(is.function(signals)),
        private$add_activity(UnTrap__new(signals)),
        private$add_activity(UnTrap__new_func(signals, needs_attrs(signals)))
      )
    },

    wait = function() { private$add_activity(Wait__new()) },

    log = function(message) {
      message <- evaluate_value(message)
      switch(
        binarise(is.function(message)),
        private$add_activity(Log__new(message)),
        private$add_activity(Log__new_func(message, needs_attrs(message)))
      )
    }
  ),

  private = list(
    n_activities = 0,
    ptrs = NULL,
    names = NULL,

    add_activity = function(activity) {
      caller <- match.call(sys.function(-1), sys.call(-1))
      caller <- as.character(caller)[[1]]
      caller <- strsplit(caller, "$", fixed = TRUE)[[1]][[2]]
      if (!is.null(private$ptrs))
        activity_chain_(self$tail(), activity)
      private$ptrs <- c(private$ptrs, activity)
      private$names <- c(private$names, caller)
      private$n_activities <- private$n_activities + activity_get_n_(activity)
      self
    },

    get_parts = function(i, double=FALSE) {
      if (missing(i)) {
        parts <- seq_len(length(self))
      } else {
        stopifnot(length(i) <= length(self))
        if (is.null(i)) i <- 0
        if (is.logical(i)) {
          parts <- which(rep_len(i, length(self)))
        } else if (is.character(i)) {
          parts <- which(private$names %in% i)
          if (double) parts <- parts[[1]]
        } else if (is.numeric(i)) {
          i <- i[!is.na(i)]
          if (any(i < 0) && any(i > 0))
            stop("only 0's may be mixed with negative subscripts")
          i <- as.integer(i)
          i <- i[i != 0]
          if (any(i < 0))
            parts <- seq_len(length(self))[i]
          else parts <- i
        } else stop("invalid subscript type '", typeof(i), "'")
      }
      parts
    },

    clone2 = function(){},
    copy = function(deep=TRUE) { self$subset() }
  )
)
Trajectory$private_methods$clone2 <- Trajectory$public_methods$clone
Trajectory$public_methods$clone <- Trajectory$private_methods$copy
