#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

.onUnload <- function (libpath) {
  library.dynam.unload("simmer", libpath)
}

evaluate_value<-function(value){
  tryCatch(
    {
      abs(eval(parse(text=value)))
    }, 
    error = function(err) value)
}

#' Checks if attributes should be supplied
#'
#' @param variable the variable to check
#'
#' @return a boolean
#' @importFrom methods formalArgs
needs_attrs<-function(variable){
  if(is.function(variable) && length(formalArgs(variable))>0) return(TRUE)
  else return(FALSE)
}

envs_apply <- function(envs, method, ...) {
  if (!is.list(envs)) envs <- list(envs)
  args <- list(...)
  
  do.call(rbind, lapply(1:length(envs), function(i) {
    stats <- do.call(eval(parse(text=method), envs[[i]]), args)
    if (nrow(stats)) stats$replication <- i
    else cbind(stats, data.frame(replication=character()))
    stats
  }))
}

#' @importFrom utils head install.packages menu
checkInstall <- function(pkgs) {
  good <- rep(TRUE, length(pkgs))
  for (i in seq(along = pkgs)) {
    tested <- try(find.package(pkgs[i]), silent = TRUE)
    if (class(tested)[1] == "try-error") good[i] <- FALSE
  }
  if (any(!good)) {
    pkList <- paste(pkgs[!good], collapse = ", ")
    cat(paste("simmer's plotting capabilities depend on ",
              ifelse(sum(!good) > 1, "missing packages (", "a missing package ("),
              pkList,
              ").\nWould you like to try to install",
              ifelse(sum(!good) > 1, " them", " it"),
              " now?",
              sep = ""))
    if(interactive()) {
      if (menu(c("yes", "no")) == 1)
        install.packages(pkgs[!good])
      else stop()
    } else stop()
  }
}
