# Copyright (C) 2015 Iñaki Ucar and Bart Smeets
# Copyright (C) 2015-2018 Iñaki Ucar
#
# This file is part of simmer.
#
# simmer is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# simmer is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with simmer. If not, see <http://www.gnu.org/licenses/>.

#.onUnload <- function (libpath) {
#  library.dynam.unload("simmer", libpath)
#}

is_flag <- function(name, env)
  is.numeric(env[[name]]) || is.logical(env[[name]])

is_string <- function(name, env)
  is.character(env[[name]]) && length(env[[name]]) == 1

is_string_vector <- function(name, env) is.character(env[[name]])

is_number <- function(name, env) {
  if (is.numeric(env[[name]]) && length(env[[name]]) == 1) {
    if (is.infinite(env[[name]]))
      env[[name]] <- -1
    else env[[name]] <- abs(env[[name]])
    TRUE
  } else FALSE
}

is_number_vector <- function(name, env) {
  if (is.numeric(env[[name]]) && length(env[[name]]) > 1) {
    env[[name]] <- abs(env[[name]])
    TRUE
  } else FALSE
}

is_function <- function(name, env) {
  if (is.function(env[[name]])) {
    env[[name]] <- magrittr_workaround(env[[name]])
    TRUE
  } else FALSE
}

is_trajectory <- function(name, env) {
  if (name == "dots.")
    all(sapply(env[[name]], inherits, what="trajectory"))
  else inherits(env[[name]], "trajectory")
}

is_numeric <- function(name, env) is.numeric(env[[name]])
is_NA <- function(name, env) is.na(env[[name]])
is_NULL <- function(name, env) is.null(env[[name]])

get_caller <- function() {
  n <- 1; repeat {
    n <- n + 1
    caller <- try(
      match.call(sys.function(sys.parent(n)), sys.call(sys.parent(n))),
      silent = TRUE
    )
    if (inherits(caller, "try-error")) next;
    caller <- as.character(caller)[[1]]
    if (!grepl("\\$", caller)) break;
  }
  sub("\\.[[:alpha:]]+$", "", caller)
}

check_args <- function(..., env.=parent.frame()) {
  types <- list(...)
  msg <- NULL
  ns <- getNamespace("simmer")

  for (var in names(types)) {
    check <- sapply(paste0("is_", sub(" ", "_", types[[var]])), function(func) {
      if (!exists(func, ns, inherits=FALSE))
       return(inherits(env.[[var]], sub("is_", "", func)))
      do.call(ns[[func]], args=list(var, env.), envir=env.)
    })
    if (!any(check)) msg <- c(msg, paste0(
      "'", sub("dots.", "...", var), "' is not a valid ", paste0(types[[var]], collapse=" or ")))
  }

  if (length(msg))
    stop(get_caller(), ": ", paste0(msg, collapse=", "), call.=FALSE)
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

recycle <- function(param, n) {
  if (length(param) != 1 || n == 1)
    return(param)
  rep(param, n)
}
