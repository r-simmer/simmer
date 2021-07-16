# Copyright (C) 2018,2021 Iñaki Ucar
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

context("handle_unfinished")

test_that("unfinished arrivals follow the drop-out trajectory", {
  dropout <- trajectory() %>%
    timeout(1)

  t0 <- trajectory() %>%
    handle_unfinished(dropout) %>%
    seize("res") %>%
    timeout(10) %>%
    release("res")

  t1 <- trajectory() %>%
    handle_unfinished(dropout) %>%
    leave(1) %>%
    timeout(100)

  blocker <- trajectory() %>%
    set_queue_size("res", 0) %>%
    set_capacity("res", 0)

  env <- simmer(verbose = TRUE) %>%
    add_resource("res", preemptive=TRUE, queue_size_strict=TRUE) %>%
    add_generator("dummy0", t0, at(0, 0)) %>%
    add_generator("dummy1", t1, at(0)) %>%
    add_generator("blocker", blocker, at(5)) %>%
    run()

  arr <- get_mon_arrivals(env)
  res <- get_mon_arrivals(env, per_resource = TRUE)

  expect_equal(arr$start_time, c(0, 5, 0, 0))
  expect_equal(arr$end_time, c(1, 5, 6, 6))
  expect_equal(arr$activity_time, c(1, 0, 6, 1))
  expect_equal(arr$finished, rep(TRUE, 4))

  expect_equal(res$start_time, c(0, 0))
  expect_equal(res$end_time, c(5, 5))
  expect_equal(res$activity_time, c(0, 5))
})

test_that("a dropout trajectory can be unset", {
  dropout <- trajectory() %>%
    timeout(1)

  t1 <- trajectory() %>%
    handle_unfinished(dropout) %>%
    handle_unfinished(NULL) %>%
    leave(1) %>%
    timeout(100)

  env <- simmer(verbose = TRUE) %>%
    add_generator("dummy1", t1, at(0)) %>%
    run()

  arr <- get_mon_arrivals(env)

  expect_equal(arr$start_time, 0)
  expect_equal(arr$end_time, 0)
  expect_equal(arr$activity_time, 0)
  expect_equal(arr$finished, FALSE)
})

test_that("unfinished arrivals coming from a queue are restarted", {
  prio <- 0
  dropout <- trajectory() %>%
    set_prioritization(function() {
      prio <<- prio + 1
      c(prio, NA, NA)
    }, mod="+") %>%
    rollback(2)

  t1 <- trajectory() %>%
    handle_unfinished(dropout) %>%
    seize("res") %>%
    timeout(1) %>%
    release("res")

  t2 <- trajectory() %>%
    timeout(5) %>%
    # flush queue and restore
    set_queue_size("res", 0) %>%
    set_queue_size("res", Inf) %>%
    timeout(5) %>%
    # open the server
    set_capacity("res", 1)

  env <- simmer(verbose = TRUE) %>%
    add_resource("res", 0, queue_size_strict=TRUE) %>%
    add_generator("dummy", t1, at(0, 0, 0)) %>%
    add_generator("trigger", t2, at(0)) %>%
    run()

  arr <- get_mon_arrivals(env)
  res <- get_mon_arrivals(env, per_resource = TRUE)

  expect_equal(arr$start_time, c(0, 0, 0, 0))
  expect_equal(arr$end_time, 10:13)
  expect_equal(arr$activity_time, c(10, 1, 1, 1))
  expect_equal(arr$finished, rep(TRUE, 4))

  expect_equal(res$name, paste0("dummy", c(2:0, 2:0)))
  expect_equal(res$start_time, c(0, 0, 0, 5, 5, 5))
  expect_equal(res$end_time, c(5, 5, 5, 11, 12, 13))
  expect_equal(res$activity_time, c(0, 0, 0, 1, 1, 1))
})
