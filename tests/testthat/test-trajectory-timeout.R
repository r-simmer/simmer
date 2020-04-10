# Copyright (C) 2015 Iñaki Ucar and Bart Smeets
# Copyright (C) 2015-2018,2020 Iñaki Ucar
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

context("timeout")

test_that("incorrect types fail", {
  expect_error(trajectory() %>% timeout("dummy"))
  expect_error(trajectory() %>% timeout_from_attribute(2))
})

test_that("a task function that returns a non-numeric value fails", {
  t0 <- trajectory() %>%
    timeout(function() {})

  env <- simmer(verbose = TRUE) %>%
    add_generator("entity", t0, function() 1)

  expect_error(run(env))
})

test_that("a missing value fails", {
  t0 <- trajectory() %>%
    timeout(NaN)
  t1 <- trajectory() %>%
    timeout_from_attribute("asdf")

  env0 <- simmer(verbose = TRUE) %>%
    add_generator("dummy", t0, at(0))
  env1 <- simmer(verbose = TRUE) %>%
    add_generator("dummy", t1, at(0))

  expect_error(trajectory() %>% timeout(NA))
  expect_error(run(env0))
  expect_error(run(env1))
})

test_that("a timeout is correctly monitored", {
  t <- trajectory() %>%
    set_attribute("three", 3) %>%
    set_global("minusthree", -3) %>%
    seize("dummy") %>%
    timeout(-3) %>%
    timeout(3) %>%
    timeout(function() 4) %>%
    timeout_from_attribute("three") %>%
    timeout_from_global("minusthree") %>%
    release("dummy")

  env <- simmer(verbose = TRUE) %>%
    add_generator("entity", t, at(0)) %>%
    add_resource("dummy") %>%
    run()

  expect_equal(get_mon_arrivals(env)[1, ]$end_time, 16)
  expect_equal(get_mon_arrivals(env, TRUE)[1, ]$end_time, 16)
})

test_that("an infinite timeout can be defined", {
  t <- trajectory() %>%
    timeout(Inf)

  arr <- simmer() %>%
    add_generator("dummy", t, at(0)) %>%
    run() %>%
    get_mon_arrivals()

  expect_equal(arr$end_time, Inf)
})

test_that("a null timeout is processed in the last place", {
  # custom service policy
  custom <- trajectory() %>%
    set_attribute("arrival time", function() now(env)) %>%
    renege_if(
      "recompute priority",
      out = trajectory() %>%
        # e.g., increase priority if wait_time < 3
        set_prioritization(function() {
          if (now(env) - get_attribute(env, "arrival time") < 3)
            c(1, NA, NA)     # only change the priority
          else c(NA, NA, NA) # don't change anything
        }, mod="+") %>%
        # go 2 steps back to renege_if
        rollback(2)) %>%
    seize("resource") %>%
    renege_abort() %>%
    timeout(5) %>%
    # trigger this before releasing the resource
    send("recompute priority") %>%
    timeout(0) %>%
    release("resource")

  env <- simmer(verbose=TRUE) %>%
    add_resource("resource") %>%
    add_generator("dummy", custom, at(0:4))

  run(env)
  arr <- get_mon_arrivals(env)

  expect_equal(arr$start_time, c(0, 3, 4, 1, 2))
  expect_equal(arr$end_time, seq(5, 25, 5))
  expect_equal(arr$activity_time, rep(5, 5))
  expect_true(all(arr$finished))
})
