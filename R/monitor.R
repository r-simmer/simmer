Monitor <- R6Class("monitor",
  public = list(
    initialize = function(name, ptr, get_arrivals, get_attributes, get_resources) {
      check_args(
        name = "string",
        ptr = "externalptr",
        get_arrivals = "function",
        get_attributes = "function",
        get_resources = "function"
      )
      private$name <- name
      private$ptr <- ptr
      self$get_arrivals <- function(...) get_arrivals(private$ptr, ...)
      self$get_attributes <- function(...) get_attributes(private$ptr, ...)
      self$get_resources <- function(...) get_resources(private$ptr, ...)
      self
    },

    get_arrivals = NULL,
    get_attributes = NULL,
    get_resources = NULL,

    print = function() {
      cat(paste0("simmer monitor: ", private$name, "\n"))
      invisible(self)
    },

    get_ptr = function() { private$ptr }
  ),

  private = list(
    name = NA,
    ptr = NULL
  )
)

#' Create a Monitor
#'
#' @name monitor
#' @export
monitor_mem <- function()
  Monitor$new("in memory", MemMonitor__new(), get_arrivals_, get_attributes_, get_resources_)

#' @rdname monitor
#' @export
monitor_csv <- function(path=tempdir(), reader=read.csv, sep=",", ...) {
  check_args(path="string", reader="function", sep="string")

  if (!dir.exists(path))
    stop(get_caller(), ": directory '", path, "' does not exist", call.=FALSE)

  files <- c(
    tempfile("ends_", path, ".csv"),
    tempfile("releases_", path, ".csv"),
    tempfile("attributes_", path, ".csv"),
    tempfile("resources_", path, ".csv")
  )

  Monitor$new(
    "to disk (CSV)",
    CsvMonitor__new(files[1], files[2], files[3], files[4], sep),
    function(ptr, per_resource)
      do.call(reader, list(ifelse(!per_resource, files[1], files[2])), ...),
    function(ptr) do.call(reader, list(files[3], ...)),
    function(ptr) do.call(reader, list(files[4], ...))
  )
}
