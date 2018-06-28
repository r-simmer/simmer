# Copyright (C) 2016-2018 Iñaki Ucar
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

context("clone/synchronize")

test_that("each clone follows a trajectory (1)", {
  t <- trajectory() %>%
    batch(1) %>%
    clone(3,
          trajectory("original"),
          trajectory("clone 1") %>%
            timeout(1),
          trajectory("clone 2") %>%
            timeout(2)) %>%
    timeout(0.5)

  arrivals <- simmer(verbose = TRUE) %>%
    add_generator("arrival", t, at(0)) %>%
    run() %>%
    get_mon_arrivals()

  expect_equal(arrivals$activity_time, c(0.5, 1.5, 2.5))
  expect_equal(arrivals$finished, rep(TRUE, 3))
})

test_that("each clone follows a trajectory (2)", {
  t <- trajectory() %>%
    batch(1) %>%
    clone(function() 3,
          trajectory("original") %>%
            timeout(1),
          trajectory("clone 1") %>%
            timeout(2)) %>%
    timeout(0.5)

  arrivals <- simmer(verbose = TRUE) %>%
    add_generator("arrival", t, at(0)) %>%
    run() %>%
    get_mon_arrivals()

  expect_equal(arrivals$activity_time, c(0.5, 1.5, 2.5))
  expect_equal(arrivals$finished, rep(TRUE, 3))
})

test_that("each clone follows a trajectory (3)", {
  t <- trajectory() %>%
    batch(1) %>%
    clone(3,
          trajectory("original") %>%
            timeout(1)) %>%
    timeout(0.5)

  arrivals <- simmer(verbose = TRUE) %>%
    add_generator("arrival", t, at(0)) %>%
    run() %>%
    get_mon_arrivals()

  expect_equal(arrivals$activity_time, c(0.5, 0.5, 1.5))
  expect_equal(arrivals$finished, rep(TRUE, 3))
})

test_that("each clone follows a trajectory (4)", {
  t <- trajectory() %>%
    batch(1) %>%
    clone(function() 3) %>%
    timeout(0.5)

  arrivals <- simmer(verbose = TRUE) %>%
    add_generator("arrival", t, at(0)) %>%
    run() %>%
    get_mon_arrivals()

  expect_equal(arrivals$activity_time, c(0.5, 0.5, 0.5))
  expect_equal(arrivals$finished, rep(TRUE, 3))
})

test_that("clones synchonize with the last (1)", {
  t <- trajectory() %>%
    batch(1) %>%
    clone(3,
          trajectory("original") %>%
            timeout(1),
          trajectory("clone 1") %>%
            timeout(2),
          trajectory("clone 2") %>%
            timeout(3)) %>%
    synchronize(wait = TRUE, mon_all = FALSE) %>%
    timeout(0.5)

  arrivals <- simmer(verbose = TRUE) %>%
    add_generator("arrival", t, at(0)) %>%
    run() %>%
    get_mon_arrivals()

  expect_equal(arrivals$activity_time, 3.5)
  expect_equal(arrivals$finished, TRUE)
})

test_that("clones synchonize with the last (2)", {
  t <- trajectory() %>%
    batch(1) %>%
    clone(3,
          trajectory("original") %>%
            timeout(1),
          trajectory("clone 1") %>%
            timeout(2),
          trajectory("clone 2") %>%
            timeout(3)) %>%
    synchronize(wait = TRUE, mon_all = TRUE) %>%
    timeout(0.5)

  arrivals <- simmer(verbose = TRUE) %>%
    add_generator("arrival", t, at(0)) %>%
    run() %>%
    get_mon_arrivals()

  expect_equal(arrivals$activity_time, c(1, 2, 3.5))
  expect_equal(arrivals$finished, rep(TRUE, 3))
})

test_that("clones synchonize with the first (1)", {
  t <- trajectory() %>%
    batch(1) %>%
    clone(3,
          trajectory("original") %>%
            timeout(1),
          trajectory("clone 1") %>%
            timeout(2),
          trajectory("clone 2") %>%
            timeout(3)) %>%
    synchronize(wait = FALSE, mon_all = FALSE) %>%
    timeout(0.5)

  arrivals <- simmer(verbose = TRUE) %>%
    add_generator("arrival", t, at(0)) %>%
    run() %>%
    get_mon_arrivals()

  expect_equal(arrivals$activity_time, 1.5)
  expect_equal(arrivals$finished, TRUE)
})

test_that("clones synchonize with the first (2)", {
  t <- trajectory() %>%
    batch(1) %>%
    clone(3,
          trajectory("original") %>%
            timeout(1),
          trajectory("clone 1") %>%
            timeout(2),
          trajectory("clone 2") %>%
            timeout(3)) %>%
    synchronize(wait = FALSE, mon_all = TRUE) %>%
    timeout(0.5)

  arrivals <- simmer(verbose = TRUE) %>%
    add_generator("arrival", t, at(0)) %>%
    run() %>%
    get_mon_arrivals()

  expect_equal(arrivals$activity_time, c(1.5, 2, 3))
  expect_equal(arrivals$finished, rep(TRUE, 3))
})

test_that("synchronize does not affect other arrivals", {
  t <- trajectory() %>%
    timeout(0.5) %>%
    synchronize(wait = TRUE, mon_all = TRUE) %>%
    timeout(0.5)

  arrivals <- simmer(verbose = TRUE) %>%
    add_generator("arrival", t, at(0, 1)) %>%
    run() %>%
    get_mon_arrivals()

  expect_equal(arrivals$end_time, c(1, 2))
  expect_equal(arrivals$activity_time, c(1, 1))
  expect_equal(arrivals$finished, rep(TRUE, 2))
})

test_that("attributes are copied over", {
  increment <- function(attr, env)
    trajectory() %>%
      set_attribute(attr, function() get_attribute(env, attr) + 1)

  t <- trajectory() %>%
    set_attribute("index", 1) %>%
    clone(
      n = 2,
      increment("index", env),
      increment("index", env)
    )

  env <- simmer(verbose = TRUE) %>%
    add_generator("dummy", t, at(0), mon=2)
  run(env)

  attr <- get_mon_attributes(env)

  expect_equal(attr$value, c(1, 2, 2))
})

test_that("accepts a list of trajectories", {
  t1 <- trajectory() %>%
    timeout(1)

  t2 <- trajectory() %>%
    clone(10, replicate(10, t1))

  expect_equal(length(t2), 1)
  expect_equal(get_n_activities(t2), 11)
})
