# Copyright (C) 2014-2018 Iñaki Ucar and Bart Smeets
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
      private$res <- env$get_resources()
      private$src <- env$get_sources()
      private$arrivals <- env$get_mon_arrivals(ongoing = TRUE)
      private$arrivals_res <- env$get_mon_arrivals(TRUE, ongoing = TRUE)
      private$attributes <- env$get_mon_attributes()
      private$resources <- env$get_mon_resources()
      for (name in names(private$src)) {
        private$n_generated[[name]] <- env$get_n_generated(name)
      }
      for (name in names(private$res)) {
        private$capacity[[name]] <- env$get_capacity(name)
        private$queue_size[[name]] <- env$get_queue_size(name)
        private$server_count[[name]] <- env$get_server_count(name)
        private$queue_count[[name]] <- env$get_queue_count(name)
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
          na.omit(private$arrivals_res)
        else private$arrivals_res
      } else {
        if (!ongoing)
          na.omit(private$arrivals)
        else private$arrivals
      }
    },
    get_mon_attributes = function() { private$attributes },
    get_mon_resources = function() { private$resources },
    get_n_generated = function(source) {
      if (!(source %in% names(private$src)))
        stop("source '", source, "' not found")
      private$n_generated[[source]]
    },
    get_capacity = function(resource) {
      if (!(resource %in% names(private$res)))
        stop("resource '", resource, "' not found")
      private$capacity[[resource]]
    },
    get_queue_size = function(resource) {
      if (!(resource %in% names(private$res)))
        stop("resource '", resource, "' not found")
      private$queue_size[[resource]]
    },
    get_server_count = function(resource) {
      if (!(resource %in% names(private$res)))
        stop("resource '", resource, "' not found")
      private$server_count[[resource]]
    },
    get_queue_count = function(resource) {
      if (!(resource %in% names(private$res)))
        stop("resource '", resource, "' not found")
      private$queue_count[[resource]]
    }
  ),

  private = list(
    now_val = NA,
    peek_val = NA,
    res = NA,
    src = NA,
    arrivals = NA,
    arrivals_res = NA,
    attributes = NA,
    resources = NA,
    n_generated = list(),
    capacity = list(),
    queue_size = list(),
    server_count = list(),
    queue_count = list()
  )
)
