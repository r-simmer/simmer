# Copyright (C) 2018 Iñaki Ucar
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

context("set_trajectory/set_source")

test_that("we can set a new trajectory", {
  t <- trajectory() %>%
    set_trajectory("dummy_gen", trajectory() %>% timeout(2)) %>%
    set_trajectory("dummy_df", trajectory() %>% timeout(2)) %>%
    timeout(4) %>%
    set_trajectory("dummy_gen", trajectory()) %>%
    set_trajectory("dummy_df", trajectory())

  env <- simmer(verbose = TRUE) %>%
    add_generator("dummy_gen", t, function() 1) %>%
    add_dataframe("dummy_df", t, data.frame(time=rep(1, 20)), batch=1) %>%
    run(10)
  arr <- get_mon_arrivals(env)
  arr <- arr[order(arr$start_time),]

  expect_equal(arr$start_time, rep(1:9, each=2))
  expect_equal(arr$activity_time, c(rep(4, 2), rep(2, 8), rep(0, 8)))
})

test_that("we can set a new source", {
  t <- trajectory() %>%
    set_source("dummy_gen", function() 2) %>%
    set_source("dummy_df", data.frame(time=rep(2, 20)))

  env <- simmer(verbose = TRUE) %>%
    add_generator("dummy_gen", t, function() 1) %>%
    add_dataframe("dummy_df", t, data.frame(time=rep(1, 20)), batch=1) %>%
    run(10)
  arr <- get_mon_arrivals(env)

  expect_equal(arr$start_time, rep(c(1, 3, 5, 7, 9), each=2))
})

test_that("other activities cannot modify the behaviour", {
  t1 <- trajectory() %>%
    timeout(1)

  t2 <- trajectory() %>%
    set_source(function() "dummy_gen", function() 1) %>%
    set_source(function() "dummy_df", data.frame(time=rep(1, 20))) %>%
    set_trajectory("dummy_gen", t1) %>%
    set_trajectory("dummy_df", t1) %>%
    timeout(2)

  t3 <- trajectory() %>%
    set_attribute("asdf", 1) %>%
    set_source("dummy_gen", function() 1) %>%
    set_source("dummy_df", data.frame(time=rep(1, 20))) %>%
    set_trajectory(function() "dummy_gen", t1) %>%
    set_trajectory(function() "dummy_df", t1) %>%
    timeout(2)

  arr2 <- simmer(verbose=TRUE) %>%
    add_generator("dummy_gen", t2, function() 2) %>%
    add_dataframe("dummy_df", t2, data.frame(time=rep(2, 20)), batch=1) %>%
    run(10) %>%
    get_mon_arrivals()

  arr3 <- simmer(verbose=TRUE) %>%
    add_generator("dummy_gen", t3, function() 2) %>%
    add_dataframe("dummy_df", t3, data.frame(time=rep(2, 20)), batch=1) %>%
    run(10) %>%
    get_mon_arrivals()

  expect_true(all(arr2 == arr3))
})

test_that("setting the wrong source fails", {
  t <- trajectory() %>%
    set_source("dummy", data.frame(time=rep(2, 20)))

  expect_error(
    simmer(verbose = TRUE) %>%
      add_generator("dummy", t, function() 1) %>%
      run(10)
  )

  t <- trajectory() %>%
    set_source("dummy", function() 2)

  expect_error(
    simmer(verbose = TRUE) %>%
      add_dataframe("dummy", t, data.frame(time=rep(1, 20)), batch=1) %>%
      run(10)
  )

  DF <- data.frame(
    time = rep(1, 20),
    priority = rep(1, 20),
    preemptible = rep(1, 20),
    restart = rep(0, 20),
    attr1 = 1:20
  )

  expect_error(
    simmer(verbose = TRUE) %>%
      add_dataframe(
        "dummy",
        trajectory() %>% set_source("dummy", DF[-1]),
        DF, batch=1, col_preemptible="preemptible") %>%
      run(10),
    "time"
  )

  expect_error(
    simmer(verbose = TRUE) %>%
      add_dataframe(
        "dummy",
        trajectory() %>% set_source("dummy", DF[-2]),
        DF, batch=1, col_preemptible="preemptible") %>%
      run(10),
    "priority"
  )

  expect_error(
    simmer(verbose = TRUE) %>%
      add_dataframe(
        "dummy",
        trajectory() %>% set_source("dummy", DF[-3]),
        DF, batch=1, col_preemptible="preemptible") %>%
      run(10),
    "preemptible"
  )

  expect_error(
    simmer(verbose = TRUE) %>%
      add_dataframe(
        "dummy",
        trajectory() %>% set_source("dummy", DF[-4]),
        DF, batch=1, col_preemptible="preemptible") %>%
      run(10),
    "restart"
  )

  expect_error(
    simmer(verbose = TRUE) %>%
      add_dataframe(
        "dummy",
        trajectory() %>% set_source("dummy", DF[-5]),
        DF, batch=1, col_preemptible="preemptible") %>%
      run(10),
    "attr1"
  )
})
