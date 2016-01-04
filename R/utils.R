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
