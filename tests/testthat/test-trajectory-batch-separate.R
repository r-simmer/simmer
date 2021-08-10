# Copyright (C) 2016-2017,2019,2021 Iñaki Ucar
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

counter <- function() {
  n <- -1
  function() {
    n <<- n + 1
    n
  }
}

test_that("arrivals are batched", {
  t <- trajectory(verbose = TRUE) %>%
    batch(2, timeout = 0, permanent = FALSE, rule = NULL) %>%
    seize("dummy", 1) %>%
    timeout(1) %>%
    release("dummy", 1) %>%
    #separate() %>%
    timeout(counter())

  env <- simmer(verbose = TRUE) %>%
    add_resource("dummy", 1, 0) %>%
    add_generator("arrival", t, at(0, 1, 2, 3)) %>%
    run()

  arr_glb <- get_mon_arrivals(env, per_resource = FALSE)
  arr_res <- get_mon_arrivals(env, per_resource = TRUE)
  env %>% reset()

  expect_equal(arr_glb$start_time, c(0, 1, 2, 3))
  expect_equal(arr_glb$end_time, c(2, 2, 5, 5))
  expect_equal(arr_glb$activity_time, c(1, 1, 2, 2))
  expect_equal(arr_res$start_time, c(1, 1, 3, 3))
  expect_equal(arr_res$end_time, c(2, 2, 4, 4))
  expect_equal(arr_res$activity_time, c(1, 1, 1, 1))
})

test_that("batches are separated", {
  t <- trajectory(verbose = TRUE) %>%
    batch(2, timeout = function() 0, permanent = FALSE, rule = NULL) %>%
    seize("dummy", 1) %>%
    timeout(1) %>%
    release("dummy", 1) %>%
    separate() %>%
    timeout(counter())

  env <- simmer(verbose = TRUE) %>%
    add_resource("dummy", 1, 0) %>%
    add_generator("arrival", t, at(0, 1, 2, 3)) %>%
    run()

  arr_glb <- get_mon_arrivals(env, per_resource = FALSE)
  arr_res <- get_mon_arrivals(env, per_resource = TRUE)

  expect_equal(arr_glb$start_time, c(0, 1, 2, 3))
  expect_equal(arr_glb$end_time, c(2, 3, 6, 7))
  expect_equal(arr_glb$activity_time, c(1, 2, 3, 4))
  expect_equal(arr_res$start_time, c(1, 1, 3, 3))
  expect_equal(arr_res$end_time, c(2, 2, 4, 4))
  expect_equal(arr_res$activity_time, c(1, 1, 1, 1))
})

test_that("permanent batches are NOT separated", {
  t <- trajectory(verbose = TRUE) %>%
    batch(function() 2, timeout = 0, permanent = TRUE, rule = NULL) %>%
    seize("dummy", 1) %>%
    timeout(1) %>%
    release("dummy", 1) %>%
    separate() %>%
    timeout(counter())

  env <- simmer(verbose = TRUE) %>%
    add_resource("dummy", 1, 0) %>%
    add_generator("arrival", t, at(0, 1, 2, 3)) %>%
    run()

  arr_glb <- get_mon_arrivals(env, per_resource = FALSE)
  arr_res <- get_mon_arrivals(env, per_resource = TRUE)

  expect_equal(arr_glb$start_time, c(0, 1, 2, 3))
  expect_equal(arr_glb$end_time, c(2, 2, 5, 5))
  expect_equal(arr_glb$activity_time, c(1, 1, 2, 2))
  expect_equal(arr_res$start_time, c(1, 1, 3, 3))
  expect_equal(arr_res$end_time, c(2, 2, 4, 4))
  expect_equal(arr_res$activity_time, c(1, 1, 1, 1))
})

test_that("a rule can prevent batching", {
  t <- trajectory(verbose = TRUE) %>%
    batch(2, timeout = function() 0, permanent = FALSE, rule = function() 0) %>%
    seize("dummy", 1) %>%
    timeout(1) %>%
    release("dummy", 1) %>%
    separate() %>%
    timeout(counter())

  env <- simmer(verbose = TRUE) %>%
    add_resource("dummy", 1, 0) %>%
    add_generator("arrival", t, at(0, 1, 2, 3)) %>%
    run()

  arr_glb <- get_mon_arrivals(env, per_resource = FALSE)
  arr_res <- get_mon_arrivals(env, per_resource = TRUE)

  expect_equal(arr_glb$start_time, c(0, 1, 2, 3))
  expect_equal(arr_glb$end_time, c(1, 3, 5, 7))
  expect_equal(arr_glb$activity_time, c(1, 2, 3, 4))
  expect_equal(arr_res$start_time, c(0, 1, 2, 3))
  expect_equal(arr_res$end_time, c(1, 2, 3, 4))
  expect_equal(arr_res$activity_time, c(1, 1, 1, 1))
})

test_that("a timeout can trigger early batches", {
  t <- trajectory(verbose = TRUE) %>%
    batch(function() 2, timeout = function() 0.5, permanent = FALSE, rule = NULL) %>%
    seize("dummy", 1) %>%
    timeout(1) %>%
    release("dummy", 1) %>%
    separate() %>%
    timeout(counter())

  env <- simmer(verbose = TRUE) %>%
    add_resource("dummy", 1) %>%
    add_generator("arrival", t, at(0, 1, 2, 3)) %>%
    run()

  arr_glb <- get_mon_arrivals(env, per_resource = FALSE)
  arr_res <- get_mon_arrivals(env, per_resource = TRUE)

  expect_equal(arr_glb$start_time, c(0, 1, 2, 3))
  expect_equal(arr_glb$end_time, c(1.5, 3.5, 5.5, 7.5))
  expect_equal(arr_glb$activity_time, c(1, 2, 3, 4))
  expect_equal(arr_res$start_time, c(0.5, 1.5, 2.5, 3.5))
  expect_equal(arr_res$end_time, c(1.5, 2.5, 3.5, 4.5))
  expect_equal(arr_res$activity_time, c(1, 1, 1, 1))
})

test_that("a timeout does not crash if the batch was already triggered", {
  t <- trajectory(verbose = TRUE) %>%
    batch(1, timeout = 1, permanent = FALSE, rule = NULL) %>%
    timeout(1)

  env <- simmer(verbose = TRUE) %>%
    add_resource("dummy", 1) %>%
    add_generator("arrival", t, at(0)) %>%
    run()

  arr_glb <- get_mon_arrivals(env, per_resource = FALSE)

  expect_equal(arr_glb$start_time, 0)
  expect_equal(arr_glb$end_time, 1)
  expect_equal(arr_glb$activity_time, 1)
  expect_true(arr_glb$finished)
})

test_that("a non-triggered batch does not crash if arrivals renege", {
  t <- trajectory(verbose = TRUE) %>%
    renege_in(1) %>%
    batch(2, timeout = 0, permanent = FALSE, rule = NULL)

  env <- simmer(verbose = TRUE) %>%
    add_generator("arrival", t, at(0)) %>%
    run()

  arr_glb <- get_mon_arrivals(env, per_resource = FALSE)

  expect_equal(arr_glb$start_time, 0)
  expect_equal(arr_glb$end_time, 1)
  expect_equal(arr_glb$activity_time, 0)
  expect_false(arr_glb$finished)
})

test_that("all arrivals inside a batch store an attribute change", {
  t <- trajectory(verbose = TRUE) %>%
    batch(2, timeout = 0, permanent = FALSE, rule = NULL) %>%
    set_attribute("asdf", 3) %>%
    separate()

  env <- simmer(verbose = TRUE) %>%
    add_resource("dummy", 1, 0) %>%
    add_generator("arrival", t, at(0, 1, 2, 3), mon = 2) %>%
    run()

  attr <- get_mon_attributes(env)

  expect_equal(attr$time, c(1, 1, 3, 3))
  expect_equal(attr$key, c("asdf", "asdf", "asdf", "asdf"))
  expect_equal(attr$value, c(3, 3, 3, 3))
})

test_that("a shared name in different trajectories collects arrivals in the same batch", {
  t <- trajectory(verbose = TRUE) %>%
    seize("dummy", 1) %>%
    timeout(1) %>%
    release("dummy", 1) %>%
    #separate() %>%
    timeout(counter())

  t1 <- trajectory(verbose = TRUE) %>%
    batch(2, name = "asdf") %>%
    join(t)

  t2 <- trajectory(verbose = TRUE) %>%
    batch(2, name = "asdf") %>%
    join(t)

  env <- simmer(verbose = TRUE) %>%
    add_resource("dummy", 1, 0) %>%
    add_generator("arrival0", t1, at(0, 2)) %>%
    add_generator("arrival1", t2, at(1, 3)) %>%
    run()

  arr_glb <- get_mon_arrivals(env, per_resource = FALSE)
  arr_res <- get_mon_arrivals(env, per_resource = TRUE)
  env %>% reset()

  expect_equal(arr_glb$start_time, c(0, 1, 2, 3))
  expect_equal(arr_glb$end_time, c(2, 2, 5, 5))
  expect_equal(arr_glb$activity_time, c(1, 1, 2, 2))
  expect_equal(arr_res$start_time, c(1, 1, 3, 3))
  expect_equal(arr_res$end_time, c(2, 2, 4, 4))
  expect_equal(arr_res$activity_time, c(1, 1, 1, 1))
})

test_that("unnamed batches in different trajectories collects arrivals in different batches", {
  t <- trajectory(verbose = TRUE) %>%
    seize("dummy", 1) %>%
    timeout(1) %>%
    release("dummy", 1) %>%
    #separate() %>%
    timeout(counter())

  t1 <- trajectory(verbose = TRUE) %>%
    batch(2, name = "") %>%
    join(t)

  t2 <- trajectory(verbose = TRUE) %>%
    batch(2, name = "") %>%
    join(t)

  env <- simmer(verbose = TRUE) %>%
    add_resource("dummy", 1, 0) %>%
    add_generator("arrival0", t1, at(0, 2)) %>%
    add_generator("arrival1", t2, at(1, 3)) %>%
    run()

  arr_glb <- get_mon_arrivals(env, per_resource = FALSE)
  arr_res <- get_mon_arrivals(env, per_resource = TRUE)

  expect_equal(arr_glb$start_time, c(0, 2, 1, 3))
  expect_equal(arr_glb$end_time, c(3, 3, 5, 5))
  expect_equal(arr_glb$activity_time, c(1, 1, 2, 2))
  expect_equal(arr_res$start_time, c(2, 2, 3, 3))
  expect_equal(arr_res$end_time, c(3, 3, 4, 4))
  expect_equal(arr_res$activity_time, c(1, 1, 1, 1))
})

test_that("nested batches' stats are correctly reported", {
  t <- trajectory(verbose = TRUE) %>%
    batch(2, timeout = 0, permanent = FALSE, rule = NULL) %>%
    batch(2, timeout = 0, permanent = FALSE, rule = NULL) %>%
    seize("dummy", 1) %>%
    timeout(1) %>%
    release("dummy", 1) %>%
    #separate() %>%
    #timeout(1) %>%
    #separate() %>%
    timeout(counter())

  env <- simmer(verbose = TRUE) %>%
    add_resource("dummy", 1, 0) %>%
    add_generator("arrival", t, at(0, 1, 2, 3)) %>%
    run()

  arr_glb <- get_mon_arrivals(env, per_resource = FALSE)
  arr_res <- get_mon_arrivals(env, per_resource = TRUE)

  expect_equal(arr_glb$start_time, c(0, 1, 2, 3))
  expect_equal(arr_glb$end_time, c(4, 4, 4, 4))
  expect_equal(arr_glb$activity_time, c(1, 1, 1, 1))
  expect_equal(arr_res$start_time, c(3, 3, 3, 3))
  expect_equal(arr_res$end_time, c(4, 4, 4, 4))
  expect_equal(arr_res$activity_time, c(1, 1, 1, 1))
})

test_that("nested batches are separated", {
  t <- trajectory(verbose = TRUE) %>%
    batch(2, timeout = 0, permanent = FALSE, rule = NULL) %>%
    batch(2, timeout = 0, permanent = FALSE, rule = NULL) %>%
    seize("dummy", 1) %>%
    timeout(1) %>%
    release("dummy", 1) %>%
    separate() %>%
    timeout(1) %>%
    separate() %>%
    timeout(counter())

  env <- simmer(verbose = TRUE) %>%
    add_resource("dummy", 1, 0) %>%
    add_generator("arrival", t, at(0, 1, 2, 3)) %>%
    run()

  arr_glb <- get_mon_arrivals(env, per_resource = FALSE)
  arr_res <- get_mon_arrivals(env, per_resource = TRUE)

  expect_equal(arr_glb$start_time, c(0, 1, 2, 3))
  expect_equal(arr_glb$end_time, c(5, 6, 7, 8))
  expect_equal(arr_glb$activity_time, c(2, 3, 4, 5))
  expect_equal(arr_res$start_time, c(3, 3, 3, 3))
  expect_equal(arr_res$end_time, c(4, 4, 4, 4))
  expect_equal(arr_res$activity_time, c(1, 1, 1, 1))
})

test_that("seizes across nested batches are correctly reported", {
  t <- trajectory(verbose = TRUE) %>%
    seize("dummy0", 1) %>%
    batch(1, timeout = 0, permanent = FALSE, rule = NULL) %>%
    seize("dummy1", 1) %>%
    batch(1, timeout = 0, permanent = FALSE, rule = NULL) %>%
    seize("dummy2", 1) %>%
    timeout(1) %>%
    release("dummy2", 1) %>%
    separate() %>%
    timeout(1) %>%
    release("dummy1", 1) %>%
    separate() %>%
    timeout(1) %>%
    release("dummy0", 1)

  env <- simmer(verbose = TRUE) %>%
    add_resource("dummy0", 1, 0) %>%
    add_resource("dummy1", 1, 0) %>%
    add_resource("dummy2", 1, 0) %>%
    add_generator("arrival", t, at(0)) %>%
    run()

  arr_glb <- get_mon_arrivals(env, per_resource = FALSE)
  arr_res <- get_mon_arrivals(env, per_resource = TRUE)

  expect_equal(arr_glb$start_time, 0)
  expect_equal(arr_glb$end_time, 3)
  expect_equal(arr_glb$activity_time, 3)
  expect_equal(arr_res$start_time, c(0, 0, 0))
  expect_equal(arr_res$end_time, c(1, 2, 3))
  expect_equal(arr_res$activity_time, c(1, 2, 3))
})

test_that("an infinite timeout is equivalent to a disabled timeout", {
  t <- trajectory() %>%
    batch(2, timeout = Inf)

  arr <- simmer(verbose = TRUE) %>%
    add_generator("dummy", t, at(0, 2)) %>%
    run() %>%
    get_mon_arrivals()

  expect_equal(arr$end_time, c(2, 2))
})
