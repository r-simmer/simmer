# Copyright (C) 2015-2018 Iñaki Ucar
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

context("seize/release")

test_that("resources are seized/released as expected (1)", {
  t0 <- trajectory() %>%
    seize("dummy", -1) %>%
    timeout(1) %>%
    seize("dummy", function() 2) %>%
    timeout(1) %>%
    release("dummy", -1) %>%
    timeout(1) %>%
    release("dummy", function() 2) %>%
    timeout(1) %>%
    seize("dummy", 1)

  env <- simmer(verbose = TRUE) %>%
    add_resource("dummy", 3, 0) %>%
    add_generator("arrival", t0, at(0))

  env %>% run(1)
  expect_equal(env %>% get_server_count("dummy"), 1)
  env %>% run(2)
  expect_equal(env %>% get_server_count("dummy"), 3)
  env %>% run(3)
  expect_equal(env %>% get_server_count("dummy"), 2)
  env %>% run(4)
  expect_equal(env %>% get_server_count("dummy"), 0)
})

test_that("resources are seized/released as expected (2)", {
  t0 <- trajectory() %>%
    select("dummy0", id = 0) %>%
    select(function() "dummy1", id = 1) %>%
    seize_selected(-1, id = 0) %>%
    timeout(1) %>%
    seize_selected(function() 2, id = 1) %>%
    timeout(1) %>%
    release_selected(-1, id = 0) %>%
    timeout(1) %>%
    release_selected(function() 2, id = 1) %>%
    timeout(1)

  expect_output(print(t0))

  env <- simmer(verbose = TRUE) %>%
    add_resource("dummy0", 3, 0) %>%
    add_resource("dummy1", 3, 0) %>%
    add_generator("arrival", t0, at(0))

  env %>% run(1)
  expect_equal(env %>% get_server_count("dummy0"), 1)
  env %>% run(2)
  expect_equal(env %>% get_server_count("dummy1"), 2)
  env %>% run(3)
  expect_equal(env %>% get_server_count("dummy0"), 0)
  env %>% run(4)
  expect_equal(env %>% get_server_count("dummy1"), 0)
})

test_that("a release without a previous seize fails", {
  t <- trajectory() %>%
    release("dummy", 1)

  env <- simmer(verbose = TRUE) %>%
    add_resource("dummy", 1) %>%
    add_generator("asdf", t, at(0))

  expect_error(env %>% run)
})

test_that("a release greater than seize fails", {
  t <- trajectory() %>%
    seize("dummy", 1) %>%
    release("dummy", 2)

  env <- simmer(verbose = TRUE) %>%
    add_resource("dummy", 1) %>%
    add_generator("asdf", t, at(0))

  expect_error(env %>% run)
})

test_that("incorrect types fail", {
  expect_error(trajectory() %>% seize(0, 0))
  expect_error(trajectory() %>% release(0, 0))
  expect_error(trajectory() %>% seize("dummy", "dummy"))
  expect_error(trajectory() %>% release("dummy", "dummy"))
})

test_that("arrivals perform a post.seize and then stop", {
  t <- trajectory() %>%
    seize("dummy", 1, continue = FALSE,
          post.seize = trajectory() %>%
            timeout(2) %>%
            release("dummy", 1)) %>%
    timeout(1)

  env <- simmer(verbose = TRUE) %>%
    add_resource("dummy", 1, 0) %>%
    add_generator("arrival", t, at(0)) %>%
    run()
  arrs <- env %>% get_mon_arrivals()

  expect_true(arrs$finished)
  expect_equal(arrs$activity_time, 2)
})

test_that("arrivals perform a post.seize and then stop (2)", {
  t <- trajectory() %>%
    seize("dummy", 1, continue = FALSE, post.seize = trajectory()) %>%
    timeout(1)

  env <- simmer(verbose = TRUE) %>%
    add_resource("dummy", 1, 0) %>%
    add_generator("arrival", t, at(0))

  expect_warning(run(env))

  arrs <- env %>% get_mon_arrivals()

  expect_true(arrs$finished)
  expect_equal(arrs$activity_time, 0)
})

test_that("arrivals can retry a seize", {
  t <- trajectory() %>%
    seize("dummy", 1, continue = FALSE,
          reject = trajectory() %>%
            timeout(1) %>%
            rollback(2, Inf)) %>%
    timeout(2) %>%
    release("dummy", 1)

  env <- simmer(verbose = TRUE) %>%
    add_resource("dummy", 1, 0) %>%
    add_generator("arrival", t, at(0, 1)) %>%
    run()
  arrs <- env %>% get_mon_arrivals()

  expect_equal(arrs$start_time, c(0, 1))
  expect_equal(arrs$finished, c(TRUE, TRUE))
  expect_equal(arrs$activity_time, c(2, 3))
})

test_that("an empty reject + continue=FALSE rejects but sets finished to TRUE", {
  t <- trajectory() %>%
    seize("dummy", 1, continue = FALSE, reject = trajectory()) %>%
    timeout(2) %>%
    release("dummy", 1)

  env <- simmer(verbose = TRUE) %>%
    add_resource("dummy", 1, 0) %>%
    add_generator("arrival", t, at(0, 1)) %>%
    run()
  arrs <- env %>% get_mon_arrivals()
  arrs <- arrs[order(arrs$start_time), ]

  expect_equal(arrs$start_time, c(0, 1))
  expect_equal(arrs$finished, c(TRUE, TRUE))
  expect_equal(arrs$activity_time, c(2, 0))
})

test_that("arrivals go through post.seize or reject and then continue", {
  t <- trajectory() %>%
    seize("dummy", 1, continue = TRUE,
          post.seize = trajectory() %>%
            timeout(2) %>%
            release("dummy"),
          reject = trajectory() %>%
            timeout(3)) %>%
    timeout(3)

  env <- simmer(verbose = TRUE) %>%
    add_resource("dummy", 1, 0) %>%
    add_generator("arrival", t, at(0, 1)) %>%
    run()
  arrs <- env %>% get_mon_arrivals()

  expect_equal(arrs$start_time, c(0, 1))
  expect_equal(arrs$finished, c(TRUE, TRUE))
  expect_equal(arrs$activity_time, c(5, 6))
})

test_that("leaving without releasing throws a warning (arrivals)", {
  t <- trajectory() %>%
    seize("dummy0", 2) %>%
    seize("dummy1", 1) %>%
    release("dummy0", 1)

  env <- simmer(verbose = TRUE) %>%
    add_resource("dummy0", 2) %>%
    add_resource("dummy1", 1) %>%
    add_generator("arrival", t, at(0))

  expect_warning(run(env))
})

test_that("leaving without releasing throws a warning (batches)", {
  t <- trajectory() %>%
    batch(1) %>%
    seize("dummy0", 2) %>%
    seize("dummy1", 1) %>%
    release("dummy0", 1)

  env <- simmer(verbose = TRUE) %>%
    add_resource("dummy0", 2) %>%
    add_resource("dummy1", 1) %>%
    add_generator("arrival", t, at(0))

  expect_warning(run(env))
})

test_that("arrivals don't jump the queue if there is room in the server (1)", {
  dummy <- function(n)
    trajectory() %>%
    seize("res", n) %>%
    timeout(10) %>%
    release("res", n)

  env <- simmer(verbose = TRUE) %>%
    add_resource("res", 10) %>%
    add_generator("dummy7", dummy(7), at(0, 1)) %>%
    add_generator("dummy1", dummy(1), at(2)) %>%
    run()

  arrs <- get_mon_arrivals(env)
  arrs_ordered <- arrs[order(arrs$start_time),]

  expect_equal(arrs_ordered$start_time, c(0, 1, 2))
  expect_equal(arrs_ordered$end_time, c(10, 20, 20))
  expect_equal(arrs_ordered$activity_time, c(10, 10, 10))
})

test_that("arrivals don't jump the queue if there is room in the server (2)", {
  dummy <- function(n)
    trajectory() %>%
    seize("res", n) %>%
    timeout(10) %>%
    release("res", n)

  env <- simmer(verbose = TRUE) %>%
    add_resource("res", 10, preemptive = TRUE) %>%
    add_generator("dummy7a", dummy(7), at(0)) %>%
    add_generator("dummy7b", dummy(7), at(1), priority = 1) %>%
    add_generator("dummy1", dummy(1), at(2)) %>%
    run()

  arrs <- get_mon_arrivals(env)
  arrs_ordered <- arrs[order(arrs$start_time),]

  expect_equal(arrs_ordered$start_time, c(0, 1, 2))
  expect_equal(arrs_ordered$end_time, c(20, 11, 21))
  expect_equal(arrs_ordered$activity_time, c(10, 10, 10))
})

test_that("unknown amounts can be released", {
  random <- function() sample.int(10, 1)
  t <- trajectory() %>%
    seize("res1", random) %>%
    set_attribute("res1", function() get_seized(env, "res1")) %>%
    seize("res2", random) %>%
    set_attribute("res2", function() get_seized(env, "res2")) %>%
    select("res3", id=0) %>%
    seize_selected(random, id=0) %>%
    set_attribute("res3", function() get_seized_selected(env, id=0)) %>%
    select("res4", id=1) %>%
    seize_selected(random, id=1) %>%
    set_attribute("res4", function() get_seized_selected(env, id=1)) %>%
    timeout(1) %>%
    release_all("res1") %>%
    release_selected_all(id=0) %>%
    release_all()

  env <- simmer(verbose = TRUE) %>%
    add_resource("res1", 10) %>%
    add_resource("res2", 10) %>%
    add_resource("res3", 10) %>%
    add_resource("res4", 10) %>%
    add_generator("dummy", t, at(0), mon=2)

  run(env, 1)
  expect_true(all(get_server_count(env, paste0("res", 1:4)) > rep(0, 4)))
  expect_equal(get_server_count(env, paste0("res", 1:4)),
               get_mon_attributes(env)$value)
  run(env)
  expect_equal(get_server_count(env, paste0("res", 1:4)), rep(0, 4))
})
