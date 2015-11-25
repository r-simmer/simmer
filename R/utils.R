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


#' export
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
  
