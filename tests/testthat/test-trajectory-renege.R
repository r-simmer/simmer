# Copyright (C) 2016,2018 Iñaki Ucar
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

context("renege")

test_that("an arrival in a timeout reneges (1)", {
  t <- trajectory() %>%
    renege_in(1) %>%
    timeout(4)

  env <- simmer(verbose = TRUE) %>%
    add_generator("arrival", t, at(0)) %>%
    run()

  arr <- get_mon_arrivals(env, per_resource = FALSE)
  expect_equal(arr$end_time, 1)
  expect_equal(arr$activity_time, 1)
  expect_false(arr$finished)
})

test_that("an arrival in a timeout reneges (2)", {
  t <- trajectory() %>%
    send("sig", 1) %>%
    renege_if("sig") %>%
    timeout(4)

  env <- simmer(verbose = TRUE) %>%
    add_generator("arrival", t, at(0)) %>%
    run(1000)

  arr <- get_mon_arrivals(env, per_resource = FALSE)
  expect_equal(arr$end_time, 1)
  expect_equal(arr$activity_time, 1)
  expect_false(arr$finished)
})

test_that("a reneging arrival can follow a secondary sub-trajectory (1)", {
  t <- trajectory() %>%
    renege_in(1, out = trajectory() %>% timeout(1)) %>%
    timeout(4)

  env <- simmer(verbose = TRUE) %>%
    add_generator("arrival", t, at(0)) %>%
    run()

  arr <- get_mon_arrivals(env, per_resource = FALSE)
  expect_equal(arr$end_time, 2)
  expect_equal(arr$activity_time, 2)
  expect_true(arr$finished)
})

test_that("a reneging arrival can follow a secondary sub-trajectory (2)", {
  t <- trajectory() %>%
    send("sig", 1) %>%
    renege_if("sig", out = trajectory() %>% timeout(1)) %>%
    timeout(4)

  env <- simmer(verbose = TRUE) %>%
    add_generator("arrival", t, at(0)) %>%
    run(1000)

  arr <- get_mon_arrivals(env, per_resource = FALSE)
  expect_equal(arr$end_time, 2)
  expect_equal(arr$activity_time, 2)
  expect_true(arr$finished)
})

test_that("an empty subtrajectory is equivalent to NULL (1)", {
  t <- trajectory() %>%
    renege_in(1, out = trajectory()) %>%
    timeout(4)

  env <- simmer(verbose = TRUE) %>%
    add_generator("arrival", t, at(0)) %>%
    run()

  arr <- get_mon_arrivals(env, per_resource = FALSE)
  expect_equal(arr$end_time, 1)
  expect_equal(arr$activity_time, 1)
  expect_false(arr$finished)
})

test_that("an empty subtrajectory is equivalent to NULL (2)", {
  t <- trajectory() %>%
    send("sig", 1) %>%
    renege_if("sig", out = trajectory()) %>%
    timeout(4)

  env <- simmer(verbose = TRUE) %>%
    add_generator("arrival", t, at(0)) %>%
    run(1000)

  arr <- get_mon_arrivals(env, per_resource = FALSE)
  expect_equal(arr$end_time, 1)
  expect_equal(arr$activity_time, 1)
  expect_false(arr$finished)
})

test_that("a second renege_in resets the timeout", {
  t <- trajectory() %>%
    renege_in(2) %>%
    timeout(1) %>%
    renege_in(4) %>%
    timeout(9)

  env <- simmer(verbose = TRUE) %>%
    add_generator("arrival", t, at(0)) %>%
    run()

  arr <- get_mon_arrivals(env, per_resource = FALSE)
  expect_equal(arr$end_time, 5)
  expect_equal(arr$activity_time, 5)
  expect_false(arr$finished)
})

test_that("a second renege_if resets the timeout", {
  t <- trajectory() %>%
    send("sig", 5) %>%
    renege_in(2) %>%
    timeout(1) %>%
    renege_if("sig") %>%
    timeout(9)

  env <- simmer(verbose = TRUE) %>%
    add_generator("arrival", t, at(0)) %>%
    run(1000)

  arr <- get_mon_arrivals(env, per_resource = FALSE)
  expect_equal(arr$end_time, 5)
  expect_equal(arr$activity_time, 5)
  expect_false(arr$finished)
})

test_that("a second renege_in resets the signal", {
  t <- trajectory() %>%
    send("sig", 2) %>%
    renege_if("sig") %>%
    timeout(1) %>%
    renege_in(4) %>%
    timeout(9)

  env <- simmer(verbose = TRUE) %>%
    add_generator("arrival", t, at(0)) %>%
    run(1000)

  arr <- get_mon_arrivals(env, per_resource = FALSE)
  expect_equal(arr$end_time, 5)
  expect_equal(arr$activity_time, 5)
  expect_false(arr$finished)
})

test_that("a second renege_if resets the signal", {
  t <- trajectory() %>%
    send("sig", 2) %>%
    renege_if("sig") %>%
    timeout(1) %>%
    renege_if("asdf") %>%
    timeout(9)

  env <- simmer(verbose = TRUE) %>%
    add_generator("arrival", t, at(0)) %>%
    run(1000)

  arr <- get_mon_arrivals(env, per_resource = FALSE)
  expect_equal(arr$end_time, 10)
  expect_equal(arr$activity_time, 10)
  expect_true(arr$finished)
})

test_that("reneging can be aborted (1)", {
  t <- trajectory() %>%
    renege_in(2) %>%
    timeout(1) %>%
    renege_abort() %>%
    timeout(9)

  env <- simmer(verbose = TRUE) %>%
    add_generator("arrival", t, at(0)) %>%
    run()

  arr <- get_mon_arrivals(env, per_resource = FALSE)
  expect_equal(arr$end_time, 10)
  expect_equal(arr$activity_time, 10)
  expect_true(arr$finished)
})

test_that("reneging can be aborted (2)", {
  t <- trajectory() %>%
    send("sig", 2) %>%
    renege_if("sig") %>%
    timeout(1) %>%
    renege_abort() %>%
    timeout(9)

  env <- simmer(verbose = TRUE) %>%
    add_generator("arrival", t, at(0)) %>%
    run(1000)

  arr <- get_mon_arrivals(env, per_resource = FALSE)
  expect_equal(arr$end_time, 10)
  expect_equal(arr$activity_time, 10)
  expect_true(arr$finished)
})

test_that("an arrival being served reneges (1)", {
  t <- trajectory() %>%
    renege_in(1) %>%
    seize("dummy", 1) %>%
    timeout(2) %>%
    release("dummy", 1)

  env <- simmer(verbose = TRUE) %>%
    add_resource("dummy", 1) %>%
    add_generator("arrival", t, at(0)) %>%
    run()

  arr_res <- get_mon_arrivals(env, per_resource = TRUE)
  arr_glb <- get_mon_arrivals(env, per_resource = FALSE)
  res <- get_mon_resources(env)

  expect_equal(arr_res$end_time, 1)
  expect_equal(arr_res$activity_time, 1)
  expect_equal(arr_glb$end_time, 1)
  expect_equal(arr_glb$activity_time, 1)
  expect_false(arr_glb$finished)
  expect_equal(res$time, c(0, 1))
  expect_equal(res$server, c(1, 0))
})

test_that("an arrival being served reneges (2)", {
  t <- trajectory() %>%
    renege_in(1) %>%
    seize("dummy", 1) %>%
    timeout(2) %>%
    release("dummy", 1)

  env <- simmer(verbose = TRUE) %>%
    add_resource("dummy", 1, preemptive = TRUE) %>%
    add_generator("arrival", t, at(0)) %>%
    run()

  arr_res <- get_mon_arrivals(env, per_resource = TRUE)
  arr_glb <- get_mon_arrivals(env, per_resource = FALSE)
  res <- get_mon_resources(env)

  expect_equal(arr_res$end_time, 1)
  expect_equal(arr_res$activity_time, 1)
  expect_equal(arr_glb$end_time, 1)
  expect_equal(arr_glb$activity_time, 1)
  expect_false(arr_glb$finished)
  expect_equal(res$time, c(0, 1))
  expect_equal(res$server, c(1, 0))
})

test_that("an enqueued arrival reneges", {
  t <- trajectory() %>%
    renege_in(1) %>%
    seize("dummy", 1) %>%
    renege_abort() %>%
    timeout(2) %>%
    release("dummy", 1)

  env <- simmer(verbose = TRUE) %>%
    add_resource("dummy", 1) %>%
    add_generator("arrival", t, at(0, 0)) %>%
    run()

  arr_res <- get_mon_arrivals(env, per_resource = TRUE)
  arr_glb <- get_mon_arrivals(env, per_resource = FALSE)
  res <- get_mon_resources(env)

  expect_equal(arr_res$name, c("arrival1", "arrival0"))
  expect_equal(arr_res$end_time, c(1, 2))
  expect_equal(arr_res$activity_time, c(0, 2))
  expect_equal(arr_glb$name, c("arrival1", "arrival0"))
  expect_equal(arr_glb$end_time, c(1, 2))
  expect_equal(arr_glb$activity_time, c(0, 2))
  expect_equal(arr_glb$finished, c(FALSE, TRUE))
  expect_equal(res$time, c(0, 0, 1, 2))
  expect_equal(res$server, c(1, 1, 1, 0))
  expect_equal(res$queue, c(0, 1, 0, 0))
})

test_that("a preempted arrival reneges (1)", {
  t1 <- trajectory() %>%
    seize("dummy", 1) %>%
    timeout(4) %>%
    release("dummy", 1)

  t0 <- trajectory() %>%
    renege_in(2) %>%
    join(t1)

  env <- simmer(verbose = TRUE) %>%
    add_resource("dummy", 1, preemptive = TRUE) %>%
    add_generator("arrival0", t0, at(0), priority = 0) %>%
    add_generator("arrival1", t1, at(1), priority = 1) %>%
    run()

  arr_res <- get_mon_arrivals(env, per_resource = TRUE)
  arr_glb <- get_mon_arrivals(env, per_resource = FALSE)
  res <- get_mon_resources(env)

  expect_equal(arr_res$name, c("arrival00", "arrival10"))
  expect_equal(arr_res$end_time, c(2, 5))
  expect_equal(arr_res$activity_time, c(1, 4))
  expect_equal(arr_glb$name, c("arrival00", "arrival10"))
  expect_equal(arr_glb$end_time, c(2, 5))
  expect_equal(arr_glb$activity_time, c(1, 4))
  expect_equal(arr_glb$finished, c(FALSE, TRUE))
  expect_equal(res$time, c(0, 1, 2, 5))
  expect_equal(res$server, c(1, 1, 1, 0))
  expect_equal(res$queue, c(0, 1, 0, 0))
})

test_that("a preempted arrival reneges (2)", {
  t1 <- trajectory() %>%
    seize("dummy", 1) %>%
    timeout(4) %>%
    release("dummy", 1)

  t0 <- trajectory() %>%
    renege_in(2) %>%
    join(t1)

  env <- simmer(verbose = TRUE) %>%
    add_resource("dummy", 1, preemptive = TRUE, queue_size_strict = TRUE) %>%
    add_generator("arrival0", t0, at(0), priority = 0) %>%
    add_generator("arrival1", t1, at(1), priority = 1) %>%
    run()

  arr_res <- get_mon_arrivals(env, per_resource = TRUE)
  arr_glb <- get_mon_arrivals(env, per_resource = FALSE)
  res <- get_mon_resources(env)

  expect_equal(arr_res$name, c("arrival00", "arrival10"))
  expect_equal(arr_res$end_time, c(2, 5))
  expect_equal(arr_res$activity_time, c(1, 4))
  expect_equal(arr_glb$name, c("arrival00", "arrival10"))
  expect_equal(arr_glb$end_time, c(2, 5))
  expect_equal(arr_glb$activity_time, c(1, 4))
  expect_equal(arr_glb$finished, c(FALSE, TRUE))
  expect_equal(res$time, c(0, 1, 2, 5))
  expect_equal(res$server, c(1, 1, 1, 0))
  expect_equal(res$queue, c(0, 1, 0, 0))
})

test_that("an arrival inside a batch reneges, but the batch continues", {
  t0 <- trajectory() %>%
    batch(2, name = "shared") %>%
    seize("dummy", 1) %>%
    timeout(10) %>%
    release("dummy", 1)

  t1 <- trajectory() %>%
    renege_in(5) %>%
    join(t0)

  env <- simmer(verbose = TRUE) %>%
    add_resource("dummy", 1) %>%
    add_generator("arrival0", t0, at(0)) %>%
    add_generator("arrival1", t1, at(0)) %>%
    run()

  arr_res <- get_mon_arrivals(env, per_resource = TRUE)
  arr_glb <- get_mon_arrivals(env, per_resource = FALSE)
  res <- get_mon_resources(env)

  expect_equal(arr_res$end_time, c(5, 10))
  expect_equal(arr_res$activity_time, c(5, 10))
  expect_equal(arr_glb$name, c("arrival10", "arrival00"))
  expect_equal(arr_glb$end_time, c(5, 10))
  expect_equal(arr_glb$activity_time, c(5, 10))
  expect_equal(arr_glb$finished, c(FALSE, TRUE))
  expect_equal(res$time, c(0, 10))
  expect_equal(res$server, c(1, 0))
  expect_equal(res$queue, c(0, 0))
})

test_that("the only arrival inside a batch reneges, and the batch stops", {
  t0 <- trajectory() %>%
    batch(1) %>%
    batch(1) %>%
    seize("dummy", 1) %>%
    timeout(10) %>%
    release("dummy", 1)

  t1 <- trajectory() %>%
    renege_in(5) %>%
    join(t0)

  env <- simmer(verbose = TRUE) %>%
    add_resource("dummy", 1) %>%
    add_generator("arrival1", t1, at(0)) %>%
    run()

  arr_res <- get_mon_arrivals(env, per_resource = TRUE)
  arr_glb <- get_mon_arrivals(env, per_resource = FALSE)
  res <- get_mon_resources(env)

  expect_equal(arr_res$end_time, 5)
  expect_equal(arr_res$activity_time, 5)
  expect_equal(arr_glb$end_time, 5)
  expect_equal(arr_glb$activity_time, 5)
  expect_equal(arr_glb$finished, FALSE)
  expect_equal(res$time, c(0, 5))
  expect_equal(res$server, c(1, 0))
  expect_equal(res$queue, c(0, 0))
})

test_that("a permanent batch prevents reneging", {
  t0 <- trajectory() %>%
    batch(1, name = "shared", permanent = TRUE) %>%
    seize("dummy", 1) %>%
    timeout(10) %>%
    release("dummy", 1)

  t1 <- trajectory() %>%
    renege_in(5) %>%
    join(t0)

  env <- simmer(verbose = TRUE) %>%
    add_resource("dummy", 1) %>%
    add_generator("arrival1", t1, at(0)) %>%
    run()

  arr_res <- get_mon_arrivals(env, per_resource = TRUE)
  arr_glb <- get_mon_arrivals(env, per_resource = FALSE)
  res <- get_mon_resources(env)

  expect_equal(arr_res$end_time, 10)
  expect_equal(arr_res$activity_time, 10)
  expect_equal(arr_glb$end_time, 10)
  expect_equal(arr_glb$activity_time, 10)
  expect_equal(arr_glb$finished, TRUE)
  expect_equal(res$time, c(0, 10))
  expect_equal(res$server, c(1, 0))
  expect_equal(res$queue, c(0, 0))
})

test_that("a batch inside a batch reneges", {
  t <- trajectory() %>%
    batch(2, name = "two") %>%
    batch(1, name = "one") %>%
    seize("dummy", 1) %>%
    timeout(2) %>%
    release("dummy", 1)

  t0 <- trajectory() %>%
    batch(2) %>%
    renege_in(1) %>%
    join(t)

  t1 <- trajectory() %>%
    batch(2) %>%
    join(t)

  env <- simmer(verbose = TRUE) %>%
    add_resource("dummy", 1, 0) %>%
    add_generator("arrival0", t0, at(0, 0)) %>%
    add_generator("arrival1", t1, at(0, 0)) %>%
    run()

  get_mon_arrivals(env, per_resource = FALSE)
  get_mon_arrivals(env, per_resource = TRUE)
  get_mon_resources(env)

  arr_res <- get_mon_arrivals(env, per_resource = TRUE)
  arr_glb <- get_mon_arrivals(env, per_resource = FALSE)
  res <- get_mon_resources(env)

  expect_equal(arr_res$end_time, c(1, 1, 2, 2))
  expect_equal(arr_res$activity_time, c(1, 1, 2, 2))
  expect_equal(arr_glb$name, c("arrival00", "arrival01", "arrival10", "arrival11"))
  expect_equal(arr_glb$end_time, c(1, 1, 2, 2))
  expect_equal(arr_glb$activity_time, c(1, 1, 2, 2))
  expect_equal(arr_glb$finished, c(FALSE, FALSE, TRUE, TRUE))
  expect_equal(res$time, c(0, 2))
  expect_equal(res$server, c(1, 0))
  expect_equal(res$queue, c(0, 0))
})

test_that("seizes across nested batches are correctly reported", {
  t <- trajectory(verbose = TRUE) %>%
    renege_in(2) %>%
    seize("dummy0", 1) %>%
    batch(1) %>%
    seize("dummy1", 1) %>%
    batch(1) %>%
    seize("dummy2", 1) %>%
    timeout(10) %>%
    release("dummy2", 1) %>%
    separate() %>%
    release("dummy1", 1) %>%
    separate() %>%
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
  expect_equal(arr_glb$end_time, 2)
  expect_equal(arr_glb$activity_time, 2)
  expect_equal(arr_res$start_time, c(0, 0, 0))
  expect_equal(arr_res$end_time, c(2, 2, 2))
  expect_equal(arr_res$activity_time, c(2, 2, 2))
})

test_that("a reneging arrival keeps seized resources", {
  out <- trajectory() %>% timeout(1)

  t <- trajectory() %>%
    renege_in(1, out=out, keep_seized=TRUE) %>%
    seize("dummy") %>%
    timeout(4)

  env <- simmer(verbose = TRUE) %>%
    add_resource("dummy") %>%
    add_generator("arrival", t, at(0))

  expect_warning(run(env))

  arr <- get_mon_arrivals(env)
  res <- get_mon_resources(env)

  expect_equal(arr$end_time, 2)
  expect_equal(arr$activity_time, 2)
  expect_true(arr$finished)
  expect_equal(res$time, 0)
  expect_equal(res$server, 1)

  t <- trajectory() %>%
    renege_in(1, out=out, keep_seized=TRUE) %>%
    seize("dummy") %>%
    batch(1) %>%
    seize("other") %>%
    timeout(4)

  env <- simmer(verbose = TRUE) %>%
    add_resource("dummy") %>%
    add_resource("other", 0) %>%
    add_generator("arrival", t, at(0))

  expect_warning(run(env))

  arr <- get_mon_arrivals(env)
  res <- get_mon_resources(env)

  expect_equal(arr$end_time, 2)
  expect_equal(arr$activity_time, 1)
  expect_true(arr$finished)
  expect_equal(res$resource, c("dummy", "other", "other"))
  expect_equal(res$time, c(0, 0, 1))
  expect_equal(res$server, c(1, 0, 0))
  expect_equal(res$queue, c(0, 1, 0))
})
