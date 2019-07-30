# Copyright (C) 2016-2017 Iñaki Ucar
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

context("resource-priorities")

test_that("priority queues are adhered to", {
  t <- trajectory() %>%
    seize("server", 1) %>%
    timeout(2) %>%
    release("server", 1)

  env <- simmer(verbose = TRUE) %>%
    add_resource("server", 1) %>%
    add_generator("__nonprior", t, at(c(0, 0)), priority = 0) %>%
    add_generator("__prior", t, at(1), priority = 1) %>% # should be served second
    run()

  arrs <-
    env %>% get_mon_arrivals()

  expect_equal(arrs[arrs$name == "__prior0", ]$end_time, 4)
})

test_that("priority queues are adhered to and same level priorities are processed FIFO", {
  t <- trajectory() %>%
    seize("server", 1) %>%
    timeout(2) %>%
    release("server", 1)

  env <- simmer(verbose = TRUE) %>%
    add_resource("server", 1) %>%
    add_generator("_t0_prior", t, at(c(0, 2, 4, 6)), priority = 1) %>%
    add_generator("_t1_prior", t, at(c(1, 3, 5, 7)), priority = 1) %>%
    run()

  arrs <-
    env %>% get_mon_arrivals()

  arrs_ordered <-
    arrs[order(arrs$end_time), ]

  expect_equal(as.character(arrs_ordered$name),
               c("_t0_prior0", "_t1_prior0", "_t0_prior1", "_t1_prior1",
                 "_t0_prior2", "_t1_prior2", "_t0_prior3", "_t1_prior3"))
})

test_that("a lower priority arrival gets rejected before accessing the server", {
  t <- trajectory() %>%
    seize("dummy", 1) %>%
    timeout(10) %>%
    release("dummy", 1)

  env <- simmer(verbose = TRUE) %>%
    add_generator("p0a", t, at(0, 0)) %>%
    add_generator("p1a", t, at(2, 3), priority = 1) %>%
    add_resource("dummy", 1, 2) %>%
    run()

  arrs <- env %>% get_mon_arrivals()
  arrs_ordered <- arrs[order(arrs$name), ]

  expect_equal(as.character(arrs[!arrs$finished, ]$name), "p0a1")
  expect_equal(arrs_ordered$end_time, c(10, 3, 20, 30))
})

test_that("priority works in non-saturated finite-queue resources", {
  low_prio <- trajectory() %>%
    seize("res", 1) %>%
    timeout(10) %>%
    release("res", 1)

  high_prio <- trajectory() %>%
    seize("res", 7) %>%
    timeout(10) %>%
    release("res", 7)

  env <- simmer(verbose = TRUE) %>%
    add_resource("res", 0, 10) %>%
    add_generator("low_prio", low_prio, at(rep(0, 5))) %>%
    add_generator("high_prio", high_prio, at(1), priority = 1) %>%
    run()

  arr <- get_mon_arrivals(env)

  expect_true(all(grepl("low", arr$name)))
  expect_equal(arr$start_time, c(0, 0))
  expect_equal(arr$end_time, c(1, 1))
  expect_equal(arr$activity_time, c(0, 0))
})

test_that("out-of-range priorities are not enqueued", {
  t <- trajectory() %>%
    seize("res") %>%
    timeout(4) %>%
    release("res")

  env <- simmer(verbose=TRUE) %>%
    add_resource("res", 3, queue_priority=1) %>%
    add_generator("lprio", t, at(0, 1), priority=0) %>%
    add_generator("hprio", t, at(0, 2), priority=2) %>%
    add_generator("nprio", t, at(0, 3), priority=1) %>%
    run()

  arr <- get_mon_arrivals(env)
  arr <- arr[order(arr$start_time),]

  expect_equal(arr$start_time, c(0, 0, 0, 1, 2, 3))
  expect_equal(arr$end_time, c(4, 4, 4, 1, 8, 8))
  expect_equal(arr$activity_time, c(4, 4, 4, 0, 4, 4))
  expect_equal(arr$finished, c(TRUE, TRUE, TRUE, FALSE, TRUE, TRUE))

  env <- simmer(verbose=TRUE) %>%
    add_resource("res", 3, queue_priority=c(1, 1)) %>%
    add_generator("lprio", t, at(0, 1), priority=0) %>%
    add_generator("hprio", t, at(0, 2), priority=2) %>%
    add_generator("nprio", t, at(0, 3), priority=1) %>%
    run()

  arr <- get_mon_arrivals(env)
  arr <- arr[order(arr$start_time),]

  expect_equal(arr$start_time, c(0, 0, 0, 1, 2, 3))
  expect_equal(arr$end_time, c(4, 4, 4, 1, 2, 8))
  expect_equal(arr$activity_time, c(4, 4, 4, 0, 0, 4))
  expect_equal(arr$finished, c(TRUE, TRUE, TRUE, FALSE, FALSE, TRUE))

  expect_error(simmer() %>% add_resource("res", queue_priority=c(1, 2, 3)))
})
