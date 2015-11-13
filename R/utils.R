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
