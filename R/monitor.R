Monitor <- R6Class("monitor",
  public = list(
    name = NA,

    initialize = function(name, xptr,
                          get_arrivals, get_attributes, get_resources,
                          handlers=NULL, finalize=function() {})
    {
      check_args(
        name = "string",
        xptr = "externalptr",
        get_arrivals = "function",
        get_attributes = "function",
        get_resources = "function",
        finalize = "function"
      )
      self$name <- name
      private$xptr <- xptr
      self$get_arrivals <- function(...) get_arrivals(private$xptr, ...)
      self$get_attributes <- function(...) get_attributes(private$xptr, ...)
      self$get_resources <- function(...) get_resources(private$xptr, ...)
      self$handlers <- handlers
      self$finalize <- finalize
      self
    },

    get_arrivals = NULL,
    get_attributes = NULL,
    get_resources = NULL,
    handlers = NULL,
    finalize = NULL,

    print = function() {
      cat(paste0("simmer monitor: ", self$name, "\n"))
      for (name in names(self$handlers))
        cat(paste0("{ ", name, ": ", self$handlers[[name]], " }\n"))
      invisible(self)
    },

    get_xptr = function() { private$xptr }
  ),

  private = list(
    xptr = NULL
  )
)

#' Create a Monitor
#'
#' Methods for creating \code{monitor} objects for simulation environments.
#'
#' @return A \code{monitor} object.
#' @details The in-memory monitor is enabled by default (\code{memory_mem}),
#' and it should the fastest.
#'
#' @name monitor
#' @export
monitor_mem <- function()
  Monitor$new("in memory", MemMonitor__new(), get_arrivals_, get_attributes_, get_resources_)

#' @param path directory where files will be created (must exist).
#' @param keep whether to keep files on exit. By default, files are removed.
#' @param sep separator character.
#' @param reader function that will be used to read the files.
#' @param ... further arguments for \code{reader}.
#'
#' @details For large simulations, or if the RAM footprint is an issue, you may
#' consider monitoring to disk. To that end, \code{monitor_csv} stores the values
#' in comma-separated (CSV) files. The usual \code{\link{get_mon}_*} methods
#' retrieve data frames from such files using the \code{reader} provided. By
#' default, \code{\link[utils]{read.csv}} is used, but you may consider using
#' faster alternatives from other packages. It is also possible to \code{keep}
#' the files in a custom directory to read and post-process them in a separate
#' workflow.
#'
#' @rdname monitor
#' @importFrom utils read.csv
#' @export
monitor_csv <- function(path=tempdir(), keep=FALSE, sep=",", reader=read.csv, ...) {
  check_args(path="string", reader="function", sep="string")

  path <- path.expand(path)
  if (!dir.exists(path))
    stop("monitor_csv: directory '", path, "' does not exist", call.=FALSE)

  files <- c(
    arrivals = tempfile("arrivals_", path, ".csv"),
    releases = tempfile("releases_", path, ".csv"),
    attributes = tempfile("attributes_", path, ".csv"),
    resources = tempfile("resources_", path, ".csv")
  )
  file.create(files)

  Monitor$new(
    "to disk (CSV)",
    CsvMonitor__new(files[1], files[2], files[3], files[4], sep),
    function(xptr, per_resource)
      do.call(reader, list(ifelse(!per_resource, files[1], files[2])), ...),
    function(xptr) do.call(reader, list(files[3], ...)),
    function(xptr) do.call(reader, list(files[4], ...)),
    files,
    function() if(!keep) unlink(files)
  )
}
