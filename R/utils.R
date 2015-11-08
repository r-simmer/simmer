#' Get an activity's next activity
#'
#' @param activity a pointer to the activity
#' @export
activity.get_next <- function(activity) {
  activity_get_next_(activity)
}

#' Show an activity
#'
#' @param activity a pointer to the activity
#' @export
activity.show <- function(activity) {
  activity_show_(activity, 0)
}

evaluate_value<-function(value){
  tryCatch(
    {
      abs(eval(parse(text=value)))
    }, 
    error = function(err) value)
}
