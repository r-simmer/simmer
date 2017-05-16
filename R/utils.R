.onUnload <- function (libpath) {
  library.dynam.unload("simmer", libpath)
}

evaluate_value <- function(value) {
  value <- magrittr_workaround(value)
  if (is.null(value)) value
  else tryCatch({
    abs(eval(parse(text = value)))
  }, error = function(err) value)
}

needs_attrs <- function(variable) {
  if (is.function(variable))
    return(length(formals(variable)))
  else return(0)
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

make_resetable <- function(distribution) {
  distribution <- magrittr_workaround(distribution)
  if (identical(environment(distribution), .GlobalEnv))
    environment(distribution) <- new.env(parent = environment(distribution))
  init <- as.list(environment(distribution))
  environment(distribution)$.reset <- new.env(parent = environment(distribution))
  environment(distribution)$.reset$init <- init
  environment(distribution)$.reset$reset <- function() {
    lst <- parent.env(environment())$init
    cls <- parent.env(parent.env(environment()))
    for (i in ls(lst, all.names = TRUE)) assign(i, get(i, lst), cls)
  }
  environment(environment(distribution)$.reset$reset) <- environment(distribution)$.reset
  return(distribution)
}

binarise <- function(...) {
  args <- c(...)
  sum(2^(seq_along(args) - 1) * args) + 1
}

# see https://github.com/tidyverse/magrittr/issues/146
magrittr_workaround <- function(func) {
  if (is.function(func) && !identical(environment(func), .GlobalEnv) &&
      "." %in% ls(envir=environment(func), all.names=TRUE))
    rm(".", envir=environment(func))
  func
}
