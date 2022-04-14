# Copyright (C) 2022 Iñaki Ucar
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

report_finalization <- function(env) {
  options(simmer.finalized = NULL)
  reg.finalizer(env$sim_obj, function(x) options(simmer.finalized = TRUE))
  invisible(env)
}

test_that("bare generators do not prevent environment destruction", {
  simulate_bare_function <- function() {
    env <- report_finalization(simmer())
    env <- env %>%
      add_generator("dummy", trajectory(), function() 1) %>%
      run(5) %>%
      wrap()
    invisible()
  }

  simulate_bare_function()
  invisible(gc())
  expect_true(getOption("simmer.finalized"))
})

test_that("convenience generators do not prevent environment destruction", {
  simulate_convenience_1 <- function() {
    env <- report_finalization(simmer())
    env <- env %>%
      add_generator("dummy", trajectory(), from(1, function() 1)) %>%
      run(5) %>%
      wrap()
    invisible()
  }

  simulate_convenience_2 <- function() {
    activ <- trajectory("activator") %>%
      activate("dummy")
    env <- report_finalization(simmer())
    env <- env %>%
      add_generator("activator", activ, function() 1) %>%
      add_generator("dummy", trajectory(), when_activated(function() 1)) %>%
      run(5) %>%
      wrap()
    invisible()
  }

  simulate_convenience_1()
  invisible(gc())
  expect_true(getOption("simmer.finalized"))

  simulate_convenience_2()
  invisible(gc())
  expect_true(getOption("simmer.finalized"))
})
