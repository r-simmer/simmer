#' Print an activity
#' 
#' It can be used to visualise an activity's internal structure.
#'
#' @param activity an external pointer to the activity.
#' 
#' @seealso \code{\link{get_head}}, \code{\link{get_tail}}, 
#' \code{\link{get_next_activity}}, \code{\link{get_prev_activity}}.
#' @export
print_activity <- function(activity) activity_print_(activity, 0)

#' Get the next/prev activity
#' 
#' It takes an external pointer to an activity an returns the next/prev activity.
#'
#' @inheritParams print_activity
#' 
#' @return An external pointer to an activity object.
#' @seealso \code{\link{get_head}}, \code{\link{get_tail}}, \code{\link{print_activity}}.
#' @export
get_next_activity <- function(activity) activity_get_next_(activity)

#' @rdname get_next_activity
#' @export
get_prev_activity <- function(activity) activity_get_prev_(activity)
