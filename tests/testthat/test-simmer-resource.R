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

context("resource")

test_that("resources are correctly created", {
  env <- simmer(verbose = TRUE) %>%
    add_resource("dummy", 5, Inf)

  expect_warning(env %>% add_resource("dummy"))
  expect_error(env %>% get_capacity("asdf"))
  expect_equal(env %>% get_capacity("dummy"), 5)
  expect_error(env %>% get_queue_size("asdf"))
  expect_equal(env %>% get_queue_size("dummy"), Inf)
  expect_error(env %>% get_server_count("asdf"))
  expect_equal(env %>% get_server_count("dummy"), 0)
  expect_error(env %>% get_queue_count("asdf"))
  expect_equal(env %>% get_queue_count("dummy"), 0)
})

test_that("a negative capacity or queue_size is converted to positive", {
  env <- simmer(verbose = TRUE) %>%
    add_resource("dummy", -4, -1)

  expect_equal(env %>% get_capacity("dummy"), 4)
  expect_equal(env %>% get_queue_size("dummy"), 1)
})

test_that("a non-existent resource fails", {
  t0 <- trajectory("") %>%
    seize("dummy", 1) %>%
    release("dummy", 1)

  env <- simmer(verbose = TRUE) %>%
    add_generator("customer", t0, function() 1)

  expect_error(env %>% run())
})

test_that("resource slots are correctly filled", {
  t0 <- trajectory("") %>%
    seize("dummy", 1) %>%
    set_attribute("dummy", 1)

  env <- simmer(verbose = TRUE) %>%
    add_resource("dummy", 2, 2) %>%
    add_generator("customer", t0, at(1:5), mon = 2)

  expect_warning(run(env))

  arrivals <- env %>% get_mon_arrivals()
  arrivals_res <- env %>% get_mon_arrivals(TRUE)
  resources <- env %>% get_mon_resources()
  attributes <- env %>% get_mon_attributes()

  expect_equal(arrivals[3, ]$finished, FALSE)
  expect_equal(nrow(arrivals_res), 0)
  expect_equal(resources[4, ]$server, 2)
  expect_equal(resources[4, ]$queue, 2)
  expect_equal(sum(attributes$value), 2)
})

test_that("resources are correctly monitored 1", {
  t0 <- trajectory("") %>%
    seize("dummy", 1) %>%
    timeout(1) %>%
    release("dummy", 1)

  env <- simmer(verbose = TRUE) %>%
    add_resource("dummy", 2) %>%
    add_generator("customer", t0, at(0)) %>%
    run()

  arrivals <- env %>% get_mon_arrivals()
  arrivals_res <- env %>% get_mon_arrivals(TRUE)
  resources <- env %>% get_mon_resources()

  expect_equal(arrivals$start_time, 0)
  expect_equal(arrivals$activity_time, 1)
  expect_equal(arrivals_res$start_time, 0)
  expect_equal(arrivals_res$activity_time, 1)
  expect_equal(resources[1, ]$server, 1)
  expect_equal(resources[2, ]$server, 0)
})

test_that("resources are correctly monitored 2", {
  t0 <- trajectory("") %>%
    seize("dummy", 1) %>%
    timeout(1) %>%
    release("dummy", 1) %>%
    rollback(3, 1)

  env <- simmer(verbose = TRUE) %>%
    add_resource("dummy", 2) %>%
    add_generator("customer", t0, at(0)) %>%
    run()

  arrivals <- env %>% get_mon_arrivals()
  arrivals_res <- env %>% get_mon_arrivals(TRUE)
  resources <- env %>% get_mon_resources()

  expect_equal(arrivals$start_time, 0)
  expect_equal(arrivals$activity_time, 2)
  expect_equal(arrivals_res$start_time, c(0, 1))
  expect_equal(arrivals_res$activity_time, c(1, 1))
  expect_equal(resources[1, ]$server, 1)
  expect_equal(resources[2, ]$server, 0)
})

test_that("a big departure triggers more than one small seize from the queue", {
  t0 <- trajectory("") %>%
    seize("dummy", 2) %>%
    timeout(10) %>%
    release("dummy", 2)
  t1 <- trajectory("") %>%
    seize("dummy", 1) %>%
    timeout(10) %>%
    release("dummy", 1)

  env <- simmer(verbose = TRUE) %>%
    add_resource("dummy", 2) %>%
    add_generator("a", t0, at(0)) %>%
    add_generator("b", t1, at(1, 2)) %>%
    run()

  arrs <- env %>% get_mon_arrivals()
  arrs_ordered <- arrs[order(arrs$name), ]

  expect_equal(as.character(arrs_ordered$name), c("a0", "b0", "b1"))
  expect_equal(arrs_ordered$end_time, c(10, 20, 20))
})
