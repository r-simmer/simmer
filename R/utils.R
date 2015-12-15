#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

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
