# Copyright (C) 2015 Iñaki Ucar and Bart Smeets
# Copyright (C) 2015-2016,2018 Iñaki Ucar
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

Wrap <- R6Class("wrap",
  public = list(
    name = NA,

    initialize = function(env) {
      check_args(env="simmer")
      self$name <- env$name
      private$now_val <- env$now()
      private$peek_val <- env$peek(Inf, TRUE)

      private$resources <- env$get_resources()
      private$sources <- env$get_sources()
      private$globals <- env$get_globals()

      private$mon_arrivals <- env$get_mon_arrivals(ongoing = TRUE)
      private$mon_arrivals_res <- env$get_mon_arrivals(TRUE, ongoing = TRUE)
      private$mon_attributes <- env$get_mon_attributes()
      private$mon_resources <- env$get_mon_resources()

      sources <- names(private$sources)
      resources <- names(private$resources)
      if (!is.null(sources))
        private$n_generated[sources] <- env$get_n_generated(sources)
      if (!is.null(resources)) {
        private$capacity[resources] <- env$get_capacity(resources)
        private$queue_size[resources] <- env$get_queue_size(resources)
        private$server_count[resources] <- env$get_server_count(resources)
        private$queue_count[resources] <- env$get_queue_count(resources)
      }

      self
    },

    print = Simmer$public_methods$print,

    now = function() private$now_val,

    peek = function(steps=1, verbose=F) {
      check_args(steps="number", verbose="flag")
      steps <- min(steps, nrow(private$peek_val))
      ret <- private$peek_val[0:steps, ]
      if (!verbose) ret$time
      else ret # nocov
    },

    get_mon_arrivals = function(per_resource=FALSE, ongoing=FALSE) {
      if (per_resource) {
        if (!ongoing)
          na.omit(private$mon_arrivals_res)
        else private$mon_arrivals_res
      } else {
        if (!ongoing)
          na.omit(private$mon_arrivals)
        else private$mon_arrivals
      }
    },
    get_mon_attributes = function() { private$mon_attributes },
    get_mon_resources = function() { private$mon_resources },

    get_n_generated = function(sources)
      unlist(private$n_generated[private$check(sources)], use.names=FALSE),

    get_capacity = function(resources)
      unlist(private$capacity[private$check(resources)], use.names=FALSE),

    get_queue_size = function(resources)
      unlist(private$queue_size[private$check(resources)], use.names=FALSE),

    get_server_count = function(resources)
      unlist(private$server_count[private$check(resources)], use.names=FALSE),

    get_queue_count = function(resources)
      unlist(private$queue_count[private$check(resources)], use.names=FALSE),

    get_sources = function() { private$sources },
    get_resources = function() { private$resources }
  ),

  private = list(
    now_val = NA,
    peek_val = NA,
    resources = NA,
    sources = NA,
    globals = NA,
    mon_arrivals = NA,
    mon_arrivals_res = NA,
    mon_attributes = NA,
    mon_resources = NA,
    n_generated = list(),
    capacity = list(),
    queue_size = list(),
    server_count = list(),
    queue_count = list(),

    check = function(entities) {
      comp <- as.character(substitute(entities))
      found <- entities %in% names(private[[comp]])
      if (any(!found))
        stop(comp, " '", paste(entities[!found], collapse=", "), "' not found")
      entities
    }
  )
)
