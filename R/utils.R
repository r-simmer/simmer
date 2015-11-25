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
#' @return a function whcih allows an attrs parameter
func_wrapper<-function(value){
  if(is.function(value)){
    primary_func <- value
  } else {
    primary_func <- function() value
  }
  
  # func should allow for a parameter to which attributes are passed
  if(length(formalArgs(primary_func))==0){
    function(attrs) primary_func()
  } else {
    primary_func
  }
}
  
