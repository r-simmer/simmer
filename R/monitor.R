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
#' @param ext file extension to use.
#' @param reader function that will be used to read the files.
#' @param args a list of further arguments for \code{reader}.
#'
#' @details For large simulations, or if the RAM footprint is an issue, you may
#' consider monitoring to disk. To that end, \code{monitor_delim} stores the values
#' in flat delimited files. The usual \code{\link{get_mon}_*} methods retrieve
#' data frames from such files using the \code{reader} provided. By default,
#' \code{\link[utils:read.table]{read.delim}} is used, but you may consider using faster
#' alternatives from other packages. It is also possible to \code{keep} the
#' files in a custom directory to read and post-process them in a separate
#' workflow.
#'
#' @rdname monitor
#' @importFrom utils read.delim
#' @export
monitor_delim <- function(path=tempdir(), keep=FALSE, sep=" ", ext=".txt",
                          reader=read.delim, args=list(stringsAsFactors=FALSE))
{
  check_args(path="string", sep="string", ext="string", reader="function", args="list")

  path <- path.expand(path)
  if (!dir.exists(path))
    stop(match.call()[[1]], ": directory '", path, "' does not exist", call.=FALSE)

  pattern <- tempfile(tmpdir=path)
  files <- c(
    arrivals = paste0(pattern, "_arrivals", ext),
    releases = paste0(pattern, "_releases", ext),
    attributes = paste0(pattern, "_attributes", ext),
    resources = paste0(pattern, "_resources", ext)
  )
  file.create(files)

  Monitor$new(
    "to disk (delimited files)",
    CsvMonitor__new(files[[1]], files[[2]], files[[3]], files[[4]], sep),
    function(xptr, per_resource)
      do.call(reader, c(ifelse(!per_resource, files[[1]], files[[2]]), args)),
    function(xptr) do.call(reader, c(files[[3]], args)),
    function(xptr) do.call(reader, c(files[[4]], args)),
    files,
    function() if(!keep) unlink(files)
  )
}

#' @details \code{monitor_csv} is a special case of \code{monitor_delim} with
#' \code{sep=","} and \code{ext=".csv"}.
#'
#' @rdname monitor
#' @importFrom utils read.csv
#' @export
#' @examples
#' mon <- monitor_csv()
#' mon
#'
#' env <- simmer(mon=mon) %>%
#'   add_generator("dummy", trajectory() %>% timeout(1), function() 1) %>%
#'   run(10)
#' env
#'
#' read.csv(mon$handlers["arrivals"]) # direct access
#' get_mon_arrivals(env)              # adds the "replication" column
#'
monitor_csv <- function(path=tempdir(), keep=FALSE,
                        reader=read.csv, args=list(stringsAsFactors=FALSE))
  monitor_delim(path, keep, ",", ".csv", reader, args)
