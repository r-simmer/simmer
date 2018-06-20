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

context("method chaining")

test_that("trajectory's method chaining works", {
  t0 <- trajectory() %>%
    seize("one", 1) %>%
    release("one", 1) %>%
    timeout(function() 1) %>%
    branch(function() 1, TRUE, trajectory() %>% timeout(function() 1)) %>%
    rollback(1) %>%
    seize("one", 1)

  expect_is(t0, "trajectory")
})

test_that("simmer's method chaining works", {
  t0 <- trajectory() %>%
    timeout(function() 1)

  env <- simmer(verbose = TRUE) %>%
    add_resource("server") %>%
    add_generator("customer", t0, function() 1) %>%
    stepn() %>%
    run(10) %>%
    reset()

  expect_is(env, "simmer")
})
