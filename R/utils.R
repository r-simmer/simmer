#' @useDynLib simmer
#' @importFrom Rcpp evalCpp
#' @importFrom R6 R6Class
NULL

#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

.onUnload <- function (libpath) {
  library.dynam.unload("simmer", libpath)
}

evaluate_value <- function(value) {
  tryCatch({
      abs(eval(parse(text = value)))
    },
    error = function(err) value)
}

needs_attrs <- function(variable) {
  if (is.function(variable))
    return(length(methods::formalArgs(variable)))
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

checkInstall <- function(pkgs) {
  good <- rep(TRUE, length(pkgs))
  for (i in seq(along = pkgs)) {
    tested <- try(find.package(pkgs[i]), silent = TRUE)
    if (class(tested)[1] == "try-error") good[i] <- FALSE
  }
  if (any(!good)) {
    pklist <- paste(pkgs[!good], collapse = ", ") # nocov start
    cat(paste("simmer's plotting capabilities depend on ",
              ifelse(sum(!good) > 1, "missing packages (", "a missing package ("),
              pklist,
              ").\nWould you like to try to install",
              ifelse(sum(!good) > 1, " them", " it"),
              " now?",
              sep = ""))
    if (interactive()) {
      if (utils::menu(c("yes", "no")) == 1)
        utils::install.packages(pkgs[!good])
      else stop()
    } else stop() # nocov end
  }
}

make_resetable <- function(distribution) {
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
