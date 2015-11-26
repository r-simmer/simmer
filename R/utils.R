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


#' Wrap a function / or value to be evaluated by the Rcpp backend
#'
#' @param value should be a value or a functuon
#'
#' @return a function which allows an attrs parameter
func_wrapper<-function(value){
  if(is.function(value)){
    primary_func <- value
  } else {
    if(!is.numeric(value)) stop("Expecting a numeric value")
    primary_func <- function() value
  }
}
  
