Trajectory <- R6Class("trajectory",
  public = list(
    name = NA,
    verbose = NA,

    initialize = function(name="anonymous", verbose=FALSE) {
      check_args(name="string", verbose="flag")
      self$name <- name
      self$verbose <- verbose
      self
    },

    print = function(indent=0, verbose=self$verbose) {
      margin <- paste(rep(" ", indent), collapse = "")
      cat(paste0(margin, "trajectory: ", self$name, ", ",
                 private$n_activities, " activities\n"))
      lapply(private$ptrs, function(i) activity_print_(i, indent, verbose))
      invisible(self)
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
          n_activities <<- n_activities + activity_get_count_(new_ptr)
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
      check_args(
        resource = c("string", "NA"),
        amount = c("number", "function"),
        id = "number",
        continue = "flag",
        post.seize = c("trajectory", "NULL"),
        reject = c("trajectory", "NULL")
      )
      trj <- as.list(c(post.seize[], reject[]))
      mask <- sum(c(1, 2) * !sapply(list(post.seize, reject), is.null))
      switch(
        binarise(is.na(resource), is.function(amount)),
        private$add_activity(Seize__new(resource, amount, continue, trj, mask)),
        private$add_activity(SeizeSelected__new(id, amount, continue, trj, mask)),
        private$add_activity(Seize__new_func(resource, amount, continue, trj, mask)),
        private$add_activity(SeizeSelected__new_func(id, amount, continue, trj, mask))
      )
    },

    release = function(resource, amount=1, id=0) {
      check_args(resource=c("string", "NA"), amount=c("number", "function"), id="number")
      switch(
        binarise(is.na(resource), is.function(amount)),
        private$add_activity(Release__new(resource, amount)),
        private$add_activity(ReleaseSelected__new(id, amount)),
        private$add_activity(Release__new_func(resource, amount)),
        private$add_activity(ReleaseSelected__new_func(id, amount))
      )
    },

    set_capacity = function(resource, value, id=0) {
      check_args(resource=c("string", "NA"), value=c("numeric", "function"), id="number")
      switch(
        binarise(is.na(resource), is.function(value)),
        private$add_activity(SetCapacity__new(resource, value)),
        private$add_activity(SetCapacitySelected__new(id, value)),
        private$add_activity(SetCapacity__new_func(resource, value)),
        private$add_activity(SetCapacitySelected__new_func(id, value))
      )
    },

    set_queue_size = function(resource, value, id=0) {
      check_args(resource=c("string", "NA"), value=c("numeric", "function"), id="number")
      switch(
        binarise(is.na(resource), is.function(value)),
        private$add_activity(SetQueue__new(resource, value)),
        private$add_activity(SetQueueSelected__new(id, value)),
        private$add_activity(SetQueue__new_func(resource, value)),
        private$add_activity(SetQueueSelected__new_func(id, value))
      )
    },

    select = function(resources, policy=c("shortest-queue", "round-robin",
                                          "first-available", "random"), id=0) {
      check_args(resources=c("string vector", "function"), id="number")
      policy <- match.arg(policy)
      switch(
        binarise(is.function(resources)),
        private$add_activity(Select__new(resources, policy, id)),
        private$add_activity(Select__new_func(resources, policy, id))
      )
    },

    timeout = function(task, global=FALSE) {
      check_args(task=c("number", "function", "string"))
      if (is.character(task))
        return(private$add_activity(Timeout__new_attr(task, global)))
      switch(
        binarise(is.function(task)),
        private$add_activity(Timeout__new(task)),
        private$add_activity(Timeout__new_func(task))
      )
    },

    set_attribute = function(keys, values, global=FALSE, mod=c(NA, "+", "*")) {
      check_args(
        keys = c("string vector", "function"),
        values = c("numeric", "function"),
        global = "flag"
      )
      mod <- match.arg(mod)
      switch(
        binarise(is.function(keys), is.function(values)),
        private$add_activity(SetAttribute__new(keys, values, global, mod)),
        private$add_activity(SetAttribute__new_func1(keys, values, global, mod)),
        private$add_activity(SetAttribute__new_func2(keys, values, global, mod)),
        private$add_activity(SetAttribute__new_func3(keys, values, global, mod))
      )
    },

    activate = function(generator) {
      check_args(generator=c("string", "function"))
      switch(
        binarise(is.function(generator)),
        private$add_activity(Activate__new(generator)),
        private$add_activity(Activate__new_func(generator))
      )
    },

    deactivate = function(generator) {
      check_args(generator=c("string", "function"))
      switch(
        binarise(is.function(generator)),
        private$add_activity(Deactivate__new(generator)),
        private$add_activity(Deactivate__new_func(generator))
      )
    },

    set_trajectory = function(generator, trajectory) {
      check_args(generator=c("string", "function"), trajectory="trajectory")
      switch(
        binarise(is.function(generator)),
        private$add_activity(SetTraj__new(generator, trajectory[])),
        private$add_activity(SetTraj__new_func(generator, trajectory[]))
      )
    },

    set_distribution = function(generator, distribution) {
      check_args(generator=c("string", "function"), distribution="function")
      distribution <- make_resetable(distribution)
      switch(
        binarise(is.function(generator)),
        private$add_activity(SetDist__new(generator, distribution)),
        private$add_activity(SetDist__new_func(generator, distribution))
      )
    },

    set_prioritization = function(values) {
      check_args(values=c("number vector", "function"))
      switch(
        binarise(is.function(values)),
        private$add_activity(SetPrior__new(values)),
        private$add_activity(SetPrior__new_func(values))
      )
    },

    branch = function(option, continue, ...) {
      dots. <- list(...)
      check_args(option="function", continue="flag", dots.="trajectory")
      stopifnot(length(continue) == length(dots.))
      traj <- sapply(dots., `[`)
      private$add_activity(Branch__new(option, continue, traj))
    },

    rollback = function(amount, times=Inf, check=NULL) {
      check_args(amount="number", times="number", check=c("function", "NULL"))
      switch(
        binarise(is.function(check)),
        private$add_activity(Rollback__new(amount, times)),
        private$add_activity(Rollback__new_func(amount, check))
      )
    },

    leave = function(prob) {
      check_args(prob=c("number", "function"))
      switch(
        binarise(is.function(prob)),
        private$add_activity(Leave__new(prob)),
        private$add_activity(Leave__new_func(prob))
      )
    },

    renege_in = function(t, out=NULL) {
      check_args(t=c("number", "function"), out=c("trajectory", "NULL"))
      traj <- as.list(c(out[]))
      switch(
        binarise(is.function(t)),
        private$add_activity(RenegeIn__new(t, traj)),
        private$add_activity(RenegeIn__new_func(t, traj))
      )
    },

    renege_if = function(signal, out=NULL) {
      check_args(signal=c("string", "function"), out=c("trajectory", "NULL"))
      traj <- as.list(c(out[]))
      switch(
        binarise(is.function(signal)),
        private$add_activity(RenegeIf__new(signal, traj)),
        private$add_activity(RenegeIf__new_func(signal, traj))
      )
    },

    renege_abort = function() { private$add_activity(RenegeAbort__new()) },

    replicate = function(n, ...) {
      dots. <- list(...)
      check_args(n=c("number", "function"), dots.="trajectory")
      trj <- sapply(dots., `[`)
      switch(
        binarise(is.function(n)),
        private$add_activity(Clone__new(n, trj)),
        private$add_activity(Clone__new_func(n, trj))
      )
    },

    synchronize = function(wait=TRUE, mon_all=FALSE) {
      check_args(wait="flag", mon_all="flag")
      private$add_activity(Synchronize__new(wait, mon_all))
    },

    batch = function(n, timeout=0, permanent=FALSE, name="", rule=NULL) {
      check_args(
        n = "number",
        timeout = c("number", "function"),
        permanent = "flag",
        name = "string",
        rule = c("function", "NULL")
      )
      switch(
        binarise(is.function(timeout), is.function(rule)),
        private$add_activity(Batch__new(n, timeout, permanent, name)),
        private$add_activity(Batch__new_func1(n, timeout, permanent, name)),
        private$add_activity(Batch__new_func2(n, timeout, permanent, name, rule)),
        private$add_activity(Batch__new_func3(n, timeout, permanent, name, rule))
      )
    },

    separate = function() { private$add_activity(Separate__new()) },

    send = function(signals, delay=0) {
      check_args(signals=c("string vector", "function"), delay=c("number", "function"))
      switch(
        binarise(is.function(signals), is.function(delay)),
        private$add_activity(Send__new(signals, delay)),
        private$add_activity(Send__new_func1(signals, delay)),
        private$add_activity(Send__new_func2(signals, delay)),
        private$add_activity(Send__new_func3(signals, delay))
      )
    },

    trap = function(signals, handler=NULL, interruptible=TRUE) {
      check_args(
        signals = c("string vector", "function"),
        handler = c("trajectory", "NULL"),
        interruptible = "flag"
      )
      traj <- as.list(c(handler[]))
      switch(
        binarise(is.function(signals)),
        private$add_activity(Trap__new(signals, traj, interruptible)),
        private$add_activity(Trap__new_func(signals, traj, interruptible))
      )
    },

    untrap = function(signals) {
      check_args(signals=c("string vector", "function"))
      switch(
        binarise(is.function(signals)),
        private$add_activity(UnTrap__new(signals)),
        private$add_activity(UnTrap__new_func(signals))
      )
    },

    wait = function() { private$add_activity(Wait__new()) },

    log = function(message) {
      check_args(message=c("string", "function"))
      switch(
        binarise(is.function(message)),
        private$add_activity(Log__new(message)),
        private$add_activity(Log__new_func(message))
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
      private$n_activities <- private$n_activities + activity_get_count_(activity)
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
