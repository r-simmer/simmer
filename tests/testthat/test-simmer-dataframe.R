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

context("dataframe")

test_that("a data source name conflicts with a generator name", {
  expect_warning(
    simmer(verbose = TRUE) %>%
      add_generator("asdf", trajectory(), at(0)) %>%
      add_dataframe("asdf", trajectory(), data.frame(time=0))
  )
  expect_warning(
    simmer(verbose = TRUE) %>%
      add_dataframe("asdf", trajectory(), data.frame(time=0)) %>%
      add_generator("asdf", trajectory(), at(0))
  )
})

test_that("a data source without a trajectory fails", {
  DF <- data.frame(time=1)
  expect_error(
    simmer(verbose = TRUE) %>%
      add_dataframe("dummy", 4, DF))
})

test_that("a non-data.frame data argument fails", {
  expect_error(
    simmer(verbose = TRUE) %>%
      add_dataframe("dummy", trajectory(), 1))
})

test_that("non-existent column names fail", {
  DF <- data.frame(time=1)
  expect_error(
    simmer(verbose = TRUE) %>%
      add_dataframe("dummy", trajectory(), DF, col_time="asdf"))
})

test_that("a data source with non-numeric values fails", {
  DF <- data.frame(time=NA)
  expect_error(
    simmer(verbose = TRUE) %>%
      add_dataframe("dummy", trajectory(), DF))
  DF <- data.frame(time="asdf")
  expect_error(
    simmer(verbose = TRUE) %>%
      add_dataframe("dummy", trajectory(), DF))
})

test_that("unsorted absolute time fails", {
  expect_error(
    simmer(verbose = TRUE) %>%
      add_dataframe("dummy", trajectory(), data.frame(time=3:1), time="absolute"))
})

test_that("absolute time works as expected", {
  time <- c(0, 1, 3, 9)
  arr <- simmer(verbose=TRUE) %>%
    add_dataframe("dummy", trajectory(), data.frame(time=time), time="absolute") %>%
    run() %>%
    get_mon_arrivals()

  expect_equal(arr$start_time, time)
})

test_that("generates the expected amount", {
  env <- simmer(verbose = TRUE) %>%
    add_dataframe("dummy", trajectory(), data.frame(time=rep(1, 3))) %>%
    run()
  arr <- get_mon_arrivals(env)

  expect_error(env %>% get_n_generated("asdf"))
  expect_equal(env %>% get_n_generated("dummy"), 3)
  expect_equal(arr$start_time, 1:3)
  expect_equal(arr$end_time, 1:3)
  expect_equal(arr$activity_time, rep(0, 3))
})

test_that("data sources are reset", {
  DF <- data.frame(time=rep(1, 3))

  expect_equal(3, simmer(verbose = TRUE) %>%
    add_dataframe("dummy", trajectory(), DF) %>%
    run() %>% reset() %>% run() %>%
    get_mon_arrivals() %>% nrow()
  )
})

test_that("priorities are set", {
  t <- trajectory() %>%
    log_(function() paste(get_prioritization(env), collapse=","))

  DF <- data.frame(time=rep(1, 3), priority=1:3, preemptible=2:4, restart=c(0, 1, 0))

  env <- simmer(verbose = TRUE) %>%
    add_dataframe("dummy", t, DF, col_preemptible="preemptible")

  expect_output(run(env), "dummy0: 1,2,0.*dummy1: 2,3,1.*dummy2: 3,4,0")
})

test_that("preemptible < priority shows a warning", {
  DF <- data.frame(time=0, priority=3, preemptible=1)
  expect_warning(simmer(verbose = TRUE) %>%
    add_dataframe("dummy", trajectory(), DF, col_preemptible="preemptible") %>%
    stepn()
  )
})

test_that("attributes are set", {
  DF <- data.frame(time=rep(1, 3), attr1=1:3, attr2=3:1)

  attr <- simmer(verbose = TRUE) %>%
    add_dataframe("dummy", trajectory(), DF, mon=2, col_attributes="attr1") %>%
    run() %>%
    get_mon_attributes()

  expect_equal(attr$time, rep(0, 3))
  expect_equal(attr$name, paste0("dummy", 0:2))
  expect_equal(attr$key, rep("attr1", 3))
  expect_equal(attr$value, 1:3)

  attr <- simmer(verbose = TRUE) %>%
    add_dataframe("dummy", trajectory(), DF, mon=2) %>%
    run() %>%
    get_mon_attributes()

  expect_equal(attr$time, rep(0, 6))
  expect_equal(attr$name, rep(paste0("dummy", 0:2), each=2))
  expect_equal(attr$key, rep(paste0("attr", 1:2), 3))
  expect_equal(attr$value, c(1, 3, 2, 2, 3, 1))
})

test_that("arrival names are correctly retrieved", {
  t <- trajectory() %>%
    log_(function() get_name(env))
  DF <- data.frame(time=0)

  env <- simmer() %>%
    add_dataframe("dummy", t, DF)

  expect_output(run(env), "0: dummy0: dummy0")
  expect_error(get_name(env))
})

test_that("arrivals are correctly monitored", {
  a <- trajectory() %>%
    seize("res2", 1) %>%
    batch(1) %>%
    seize("res1", 1) %>%
    timeout(5) %>%
    release("res1", 1) %>%
    separate() %>%
    release("res2", 1)

  b <- trajectory() %>%
    seize("res1", 1) %>%
    timeout(6) %>%
    release("res1", 1)

  c <- trajectory() %>%
    seize("res1", 1) %>%
    timeout(1) %>%
    rollback(1, times = Inf)

  DFa <- DFb <- DFc <- data.frame(time=0)
  DFd <- data.frame(time=1)

  env <- simmer(verbose = TRUE) %>%
    add_resource("res1", 1) %>%
    add_resource("res2") %>%
    add_dataframe("a", a, DFa) %>%
    add_dataframe("b", b, DFb) %>%
    add_dataframe("c", c, DFc) %>%
    add_dataframe("d", c, DFd, mon = FALSE) %>%
    run(until = 4)

  arr1 <- get_mon_arrivals(env, per_resource = FALSE, ongoing = TRUE)
  arr1 <- arr1[order(arr1$name), ]
  arr2 <- get_mon_arrivals(env, per_resource = TRUE, ongoing = TRUE)
  arr2 <- arr2[order(arr2$name, arr2$resource), ]

  expect_equal(arr1$name, c("a0", "b0", "c0"))
  expect_equal(arr1$start_time, c(0, 0, 0))
  expect_equal(arr1$end_time, c(NA_real_, NA, NA))
  expect_equal(arr1$activity_time, c(NA_real_, NA, NA))
  expect_equal(arr1$finished, rep(FALSE, 3))
  expect_equal(arr2$name, c("a0", "a0", "b0", "c0"))
  expect_equal(arr2$start_time, c(0, 0, 0, 0))
  expect_equal(arr2$end_time, c(NA_real_, NA, NA, NA))
  expect_equal(arr2$activity_time, c(NA_real_, NA, NA, NA))
  expect_equal(arr2$resource, c("res1", "res2", "res1", "res1"))

  env %>%
    reset() %>%
    run(until = 10)

  arr1 <- get_mon_arrivals(env, per_resource = FALSE, ongoing = TRUE)
  arr1 <- arr1[order(arr1$name), ]
  arr2 <- get_mon_arrivals(env, per_resource = TRUE, ongoing = TRUE)
  arr2 <- arr2[order(arr2$name, arr2$resource), ]

  expect_equal(arr1$name, c("a0", "b0", "c0"))
  expect_equal(arr1$start_time, c(0, 0, 0))
  expect_equal(arr1$end_time, c(5, NA, NA))
  expect_equal(arr1$activity_time, c(5, NA, NA))
  expect_equal(arr1$finished, c(TRUE, FALSE, FALSE))
  expect_equal(arr2$name, c("a0", "a0", "b0", "c0"))
  expect_equal(arr2$start_time, c(0, 0, 0, 0))
  expect_equal(arr2$end_time, c(5, 5, NA, NA))
  expect_equal(arr2$activity_time, c(5, 5, NA, NA))
  expect_equal(arr2$resource, c("res1", "res2", "res1", "res1"))

  env %>%
    reset() %>%
    run(until = 12)

  arr1 <- get_mon_arrivals(env, per_resource = FALSE, ongoing = TRUE)
  arr1 <- arr1[order(arr1$name), ]
  arr2 <- get_mon_arrivals(env, per_resource = TRUE, ongoing = TRUE)
  arr2 <- arr2[order(arr2$name, arr2$resource), ]

  expect_equal(arr1$name, c("a0", "b0", "c0"))
  expect_equal(arr1$start_time, c(0, 0, 0))
  expect_equal(arr1$end_time, c(5, 11, NA))
  expect_equal(arr1$activity_time, c(5, 6, NA))
  expect_equal(arr1$finished, c(TRUE, TRUE, FALSE))
  expect_equal(arr2$name, c("a0", "a0", "b0", "c0"))
  expect_equal(arr2$start_time, c(0, 0, 0, 0))
  expect_equal(arr2$end_time, c(5, 5, 11, NA))
  expect_equal(arr2$activity_time, c(5, 5, 6, NA))
  expect_equal(arr2$resource, c("res1", "res2", "res1", "res1"))
})
