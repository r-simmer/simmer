Trajectory <- R6Class("trajectory",
  public = list(
    name = NA,
    verbose = NA,

    initialize = function(name="anonymous", verbose=FALSE) {
      check_args(name, verbose, types=c("string", "flag"), n=3)
      self$name <- name
      self$verbose <- verbose
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
      if (!length(continue)) continue <- TRUE
      check_args(resource, amount, id, continue, post.seize, reject,
            types=c("string or NA", "number or function", "number", "flag", rep("trajectory or NULL", 2)))
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
      check_args(resource, amount, id, types=c("string or NA", "number or function", "number"))
      switch(
        binarise(is.na(resource), is.function(amount)),
        private$add_activity(Release__new(resource, amount)),
        private$add_activity(ReleaseSelected__new(id, amount)),
        private$add_activity(Release__new_func(resource, amount, needs_attrs(amount))),
        private$add_activity(ReleaseSelected__new_func(id, amount, needs_attrs(amount)))
      )
    },

    set_capacity = function(resource, value, id=0) {
      check_args(resource, value, id, types=c("string or NA", "numeric or function", "number"))
      switch(
        binarise(is.na(resource), is.function(value)),
        private$add_activity(SetCapacity__new(resource, value)),
        private$add_activity(SetCapacitySelected__new(id, value)),
        private$add_activity(SetCapacity__new_func(resource, value, needs_attrs(value))),
        private$add_activity(SetCapacitySelected__new_func(id, value, needs_attrs(value)))
      )
    },

    set_queue_size = function(resource, value, id=0) {
      check_args(resource, value, id, types=c("string or NA", "numeric or function", "number"))
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
      check_args(resources, id, types=c("string vector or function", "number"))
      policy <- match.arg(policy)
      switch(
        binarise(is.function(resources)),
        private$add_activity(Select__new(resources, policy, id)),
        private$add_activity(Select__new_func(resources, needs_attrs(resources), policy, id))
      )
    },

    timeout = function(task) {
      check_args(task, types="number or function")
      switch(
        binarise(is.function(task)),
        private$add_activity(Timeout__new(task)),
        private$add_activity(Timeout__new_func(task, needs_attrs(task)))
      )
    },

    set_attribute = function(key, value, global=FALSE) {
      check_args(key, value, global, types=c("string", "numeric or function", "flag"))
      switch(
        binarise(is.function(value)),
        private$add_activity(SetAttribute__new(key, value, global)),
        private$add_activity(SetAttribute__new_func(key, value, needs_attrs(value), global))
      )
    },

    activate = function(generator) {
      check_args(generator, types="string or function")
      switch(
        binarise(is.function(generator)),
        private$add_activity(Activate__new(generator)),
        private$add_activity(Activate__new_func(generator, needs_attrs(generator)))
      )
    },

    deactivate = function(generator) {
      check_args(generator, types="string or function")
      switch(
        binarise(is.function(generator)),
        private$add_activity(Deactivate__new(generator)),
        private$add_activity(Deactivate__new_func(generator, needs_attrs(generator)))
      )
    },

    set_trajectory = function(generator, trajectory) {
      check_args(generator, trajectory, types=c("string or function", "trajectory"))
      switch(
        binarise(is.function(generator)),
        private$add_activity(SetTraj__new(generator, trajectory[])),
        private$add_activity(SetTraj__new_func(generator, needs_attrs(generator), trajectory[]))
      )
    },

    set_distribution = function(generator, distribution) {
      check_args(generator, distribution, types=c("string or function", "function"))
      distribution <- make_resetable(distribution)
      switch(
        binarise(is.function(generator)),
        private$add_activity(SetDist__new(generator, distribution)),
        private$add_activity(SetDist__new_func(generator, needs_attrs(generator), distribution))
      )
    },

    set_prioritization = function(values) {
      check_args(values, types="number vector or function")
      switch(
        binarise(is.function(values)),
        private$add_activity(SetPrior__new(values)),
        private$add_activity(SetPrior__new_func(values, needs_attrs(values)))
      )
    },

    branch = function(option, continue, ...) {
      check_args(option, continue, ...,
            types=c("function", "flag", rep("trajectory", length(c(...)))))
      stopifnot(length(continue) == length(c(...)))
      traj <- sapply(c(...), `[`)
      private$add_activity(Branch__new(option, needs_attrs(option), continue, traj))
    },

    rollback = function(amount, times=Inf, check=NULL) {
      check_args(amount, times, check, types=c(rep("number", 2), "function or NULL"))
      switch(
        binarise(is.function(check)),
        private$add_activity(Rollback__new(amount, times)),
        private$add_activity(Rollback__new_func(amount, check, needs_attrs(check)))
      )
    },

    leave = function(prob) {
      check_args(prob, types="number or function")
      switch(
        binarise(is.function(prob)),
        private$add_activity(Leave__new(prob)),
        private$add_activity(Leave__new_func(prob, needs_attrs(prob)))
      )
    },

    renege_in = function(t, out=NULL) {
      check_args(t, out, types=c("number or function", "trajectory or NULL"))
      traj <- as.list(c(out[]))
      switch(
        binarise(is.function(t)),
        private$add_activity(RenegeIn__new(t, traj)),
        private$add_activity(RenegeIn__new_func(t, needs_attrs(t), traj))
      )
    },

    renege_if = function(signal, out=NULL) {
      check_args(signal, out, types=c("string or function", "trajectory or NULL"))
      traj <- as.list(c(out[]))
      switch(
        binarise(is.function(signal)),
        private$add_activity(RenegeIf__new(signal, traj)),
        private$add_activity(RenegeIf__new_func(signal, needs_attrs(signal), traj))
      )
    },

    renege_abort = function() { private$add_activity(RenegeAbort__new()) },

    replicate = function(n, ...) {
      check_args(n, ..., types=c("number or function", rep("trajectory", length(c(...)))))
      trj <- sapply(c(...), `[`)
      switch(
        binarise(is.function(n)),
        private$add_activity(Clone__new(n, trj)),
        private$add_activity(Clone__new_func(n, needs_attrs(n), trj))
      )
    },

    synchronize = function(wait=TRUE, mon_all=FALSE) {
      check_args(wait, mon_all, types=rep("flag", 2))
      private$add_activity(Synchronize__new(wait, mon_all))
    },

    batch = function(n, timeout=0, permanent=FALSE, name="", rule=NULL) {
      check_args(n, timeout, permanent, name, rule,
                 types=c("number", "number or function", "flag", "string", "function or NULL"))
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
      check_args(signals, delay, types=c("string vector or function", "number or function"))
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
      check_args(signals, handler, interruptible,
                 types=c("string vector or function", "trajectory or NULL", "flag"))
      traj <- as.list(c(handler[]))
      switch(
        binarise(is.function(signals)),
        private$add_activity(Trap__new(signals, traj, interruptible)),
        private$add_activity(Trap__new_func(signals, needs_attrs(signals), traj, interruptible))
      )
    },

    untrap = function(signals) {
      check_args(signals, types="string vector or function")
      switch(
        binarise(is.function(signals)),
        private$add_activity(UnTrap__new(signals)),
        private$add_activity(UnTrap__new_func(signals, needs_attrs(signals)))
      )
    },

    wait = function() { private$add_activity(Wait__new()) },

    log = function(message) {
      check_args(message, types="string or function")
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
