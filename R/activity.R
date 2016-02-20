#' Print an activity
#' 
#' It can be used to visualise an activity's internal structure.
#'
#' @param activity an external pointer to the activity.
#' 
#' @seealso Other methods to deal with activities: 
#' \link{get_next_activity}, \link{get_prev_activity}.
#' @export
print_activity <- function(activity) activity_print_(activity, 0)

#' Get the next activity
#' 
#' It takes an external pointer to an activity an returns the next activity.
#'
#' @param activity an external pointer to the activity.
#' 
#' @return An external pointer to an activity object.
#' @seealso Other methods to deal with activities: 
#' \link{print_activity}, \link{get_prev_activity}.
#' @export
get_next_activity <- function(activity) activity_get_next_(activity)

#' Get the previous activity
#' 
#' It takes an external pointer to an activity an returns the previous activity.
#'
#' @param activity an external pointer to the activity.
#' 
#' @return An external pointer to an activity object.
#' @seealso Other methods to deal with activities: 
#' \link{print_activity}, \link{get_next_activity}.
#' @export
get_prev_activity <- function(activity) activity_get_prev_(activity)
