.onUnload <- function (libpath) {
  library.dynam.unload("simmer", libpath)
}

is_string <- function(x) is.character(x) && length(x) == 1

is_flag <- function(x) is.numeric(x) || is.logical(x)

is_number <- function(x, env, name) {
  if (is.numeric(x) && length(x) == 1) {
    if (is.infinite(x))
      env[[name]] <- -1
    else env[[name]] <- abs(x)
    TRUE
  } else FALSE
}

is_number_vector <- function(x, env, name) {
  if (is.numeric(x) && length(x) > 1) {
    env[[name]] <- abs(x)
    TRUE
  } else FALSE
}

is_function <- function(x, env, name) {
  if (is.function(x)) {
    env[[name]] <- magrittr_workaround(x)
    TRUE
  } else FALSE
}

check_args <- function(..., types, n=1, env=parent.frame()) {
  caller <- match.call(sys.function(sys.parent(n)), sys.call(sys.parent(n)))
  caller <- as.character(caller)[[1]]
  args <- list(...)
  dots <- match.call(expand.dots = FALSE)$...
  vars <- sapply(dots, deparse)
  stopifnot(length(vars) == length(types))

  msg <- NULL
  for (i in seq_along(args)) {
    x <- args[[i]]
    if (!switch(types[[i]],
      flag                        = is_flag(x),
      string                      = is_string(x),
      number                      = is_number(x, env, vars[[i]]),
      `function`                  = is_function(x, env, vars[[i]]),
      trajectory                  = inherits(x, "trajectory"),
      schedule                    = inherits(x, "schedule"),
      `string or function`        = is_string(x) || is_function(x, env, vars[[i]]),
      `string or NA`              = is_string(x) || is.na(x),
      `string vector or function` = is.character(x) || is_function(x, env, vars[[i]]),
      `number or function`        = is_number(x, env, vars[[i]]) || is_function(x, env, vars[[i]]),
      `number vector or function` = is_number_vector(x, env, vars[[i]]) || is_function(x, env, vars[[i]]),
      `numeric or function`       = is.numeric(x) || is_function(x, env, vars[[i]]),
      `number or schedule`        = is_number(x, env, vars[[i]]) || inherits(x, "schedule"),
      `function or NULL`          = is_function(x, env, vars[[i]]) || is.null(x),
      `trajectory or NULL`        = inherits(x, "trajectory") || is.null(x)
    )) msg <- c(msg, paste0("'", vars[[i]], "' is not a ", types[[i]]))
  }

  if (length(msg))
    stop(paste0(caller, ": ", paste0(msg, collapse=", ")), call. = FALSE)
}

needs_attrs <- function(variable) {
  args <- length(formals(variable))
  if (args)
    .Deprecated(msg="Attribute retrieval through function arguments is deprecated.\nUse 'get_attribute' instead.") # nocov
  args
}

envs_apply <- function(envs, method, ...) {
  if (!is.list(envs)) envs <- list(envs)
  args <- list(...)

  do.call(rbind, lapply(1:length(envs), function(i) {
    stats <- do.call(eval(parse(text = method), envs[[i]]), args)
    if (nrow(stats)) stats$replication <- i
    else cbind(stats, data.frame(replication = character()))
    stats
  }))
}

#' @importFrom codetools findGlobals
make_resetable <- function(func) {
  init <- sapply(findGlobals(func, merge=FALSE)$variables,
                 get0, envir=environment(func), simplify=FALSE)
  env <- list2env(list(init=init, env=environment(func)))
  attr(func, "reset") <- function() {
    for (i in ls(init, all.names = TRUE))
      assign(i, init[[i]], env, inherits=TRUE)
  }
  environment(attr(func, "reset")) <- env
  func
}

binarise <- function(...) {
  args <- c(...)
  sum(2^(seq_along(args) - 1) * args) + 1
}

# see https://github.com/tidyverse/magrittr/issues/146
magrittr_workaround <- function(func) {
  if (!identical(environment(func), .GlobalEnv) &&
      "." %in% ls(envir=environment(func), all.names=TRUE))
    rm(".", envir=environment(func))
  func
}
