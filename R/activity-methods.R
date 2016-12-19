#' Print an activity
#'
#' It can be used to visualise an activity's internal structure.
#'
#' @param x an external pointer to the activity.
#' @param ... arguments to be passed to or from other methods.
#'
#' @seealso \code{\link{head.trajectory}}, \code{\link{tail.trajectory}},
#' \code{\link{get_next_activity}}, \code{\link{get_prev_activity}}.
#' @export
print.externalptr <- function(x, ...) activity_print_(x, 0)

#' @param activity an external pointer to the activity.
#' @rdname print.externalptr
#' @export
print_activity <- function(activity) {
  .Deprecated("print")
  print(activity)
}

#' Get the next/prev activity
#'
#' It takes an external pointer to an activity an returns the next/prev activity.
#'
#' @param activity an external pointer to the activity.
#'
#' @return Returns an external pointer to an activity object.
#' @seealso \code{\link{head.trajectory}}, \code{\link{tail.trajectory}}, \code{\link{print.externalptr}}.
#' @export
get_next_activity <- function(activity) activity_get_next_(activity)

#' @rdname get_next_activity
#' @export
get_prev_activity <- function(activity) activity_get_prev_(activity)
