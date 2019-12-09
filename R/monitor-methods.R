# Copyright (C) 2018-2019 Iñaki Ucar
#
# This file is part of simmer.
#
# simmer is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# simmer is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with simmer. If not, see <http://www.gnu.org/licenses/>.

#' Create a Monitor
#'
#' Methods for creating \code{monitor} objects for simulation environments.
#'
#' @param name an identifier to show when printed.
#' @param xptr an external pointer pointing to a C++ object derived from the
#' abstract class simmer::Monitor. See C++ API for further details and, in
#' particular, the \code{simmer/monitor.h} header.
#' @param get_arrivals a function to retrieve the arrivals tables. It must accept
#' the \code{xptr} as a first argument, even if it is not needed, and a boolean
#' \code{per_resource} as a second argument (see \code{\link{get_mon_arrivals}}).
#' @param get_attributes a function to retrieve the attributes table. It must accept
#' the \code{xptr} as a first argument, even if it is not needed.
#' @param get_resources a function to retrieve the resources table. It must accept
#' the \code{xptr} as a first argument, even if it is not needed.
#' @param handlers an optional list of handlers that will be stored in a slot of
#' the same name. For example, \code{monitor_mem} does not use this slot, but
#' \code{monitor_delim} and \code{monitor_csv} store the path to the created files.
#' @param finalize an optional function to be called when the object is destroyed.
#' For example, \code{monitor_mem} does not require any finalizer, but
#' \code{monitor_delim} and \code{monitor_csv} use this to remove the created
#' files when the monitor is destroyed.
#'
#' @details The \code{monitor} method is a generic function to instantiate a
#' \code{monitor} object. It should not be used in general unless you want to
#' extend \code{simmer} with a custom monitor.
#'
#' @return A \code{monitor} object.
#' @export
monitor <- function(name, xptr, get_arrivals, get_attributes, get_resources,
                    handlers=NULL, finalize=function() {})
{
  check_args(name="character", xptr="externalptr", get_arrivals="function",
             get_attributes="function", get_resources="function",
             handlers=c("list", "NULL"), finalize="function")

  env <- list2env(list(
    name=name, xptr=xptr, handlers=handlers, finalize=finalize))
  env$get_arrivals <- function(...) get_arrivals(env$xptr, ...)
  env$get_attributes <- function(...) get_attributes(env$xptr, ...)
  env$get_resources <- function(...) get_resources(env$xptr, ...)

  class(env) <- "monitor"
  env
}

#' @export
print.monitor <- function(x, ...) {
  cat(paste0("simmer monitor: ", x$name, "\n"))
  for (name in names(x$handlers))
    cat(paste0("{ ", name, ": ", x$handlers[[name]], " }\n"))
  invisible(x)
}

#' @details The in-memory monitor is enabled by default (\code{memory_mem}),
#' and it should the fastest.
#'
#' @rdname monitor
#' @export
monitor_mem <- function() monitor(
  "in memory", MemMonitor__new(), get_arrivals_, get_attributes_, get_resources_)

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
  check_args(path="character", sep="character", ext="character",
             reader="function", args="list")

  path <- path.expand(path)
  if (!dir.exists(path))
    stop(match.call()[[1]], ": directory '", path, "' does not exist", call.=FALSE)

  pattern <- tempfile(tmpdir=path)
  files <- list(
    arrivals = paste0(pattern, "_arrivals", ext),
    releases = paste0(pattern, "_releases", ext),
    attributes = paste0(pattern, "_attributes", ext),
    resources = paste0(pattern, "_resources", ext)
  )
  file.create(unlist(files))

  monitor(
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
#' read.csv(mon$handlers$arrivals) # direct access
#' get_mon_arrivals(env)           # adds the "replication" column
#'
monitor_csv <- function(path=tempdir(), keep=FALSE,
                        reader=read.csv, args=list(stringsAsFactors=FALSE))
  monitor_delim(path, keep, ",", ".csv", reader, args)
