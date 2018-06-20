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

context("select")

test_that("no selection throws an error", {
  t0 <- trajectory() %>% seize_selected()

  env <- simmer() %>%
    add_resource("res") %>%
    add_generator("asdf", t0, at(0))

  expect_error(run(env))
})

test_that("core selection algorithms work: shortest-queue", {
  t0 <- trajectory() %>% seize("r1", 1)
  t1 <- trajectory() %>% seize("r2", 1)

  t2 <- trajectory() %>%
    select(c("r1", "r2", "r3"), policy = "shortest-queue") %>%
    seize_selected(1)

  env <- simmer(verbose = TRUE) %>%
    add_resource("r1", 2) %>%
    add_resource("r2", 3) %>%
    add_resource("r3", 1) %>%
    add_generator("dummy0", t0, at(0)) %>%
    add_generator("dummy1", t1, at(0, 0)) %>%
    add_generator("dummy2", t2, at(seq(1, 6)))

  expect_warning(env %>% run)

  res <- get_mon_resources(env)
  res_ordered <- res[order(res$time), ]
  res_ordered <- res_ordered[4:9, ]

  expect_equal(res_ordered$server, c(1, 2, 1, 2, 3, 1))
  expect_equal(res_ordered$queue, c(0, 0, 1, 1, 0, 2))
  expect_equal(res_ordered$resource, c("r3", "r1", "r3", "r1", "r2", "r3"))
})

test_that("core selection algorithms work: round-robin", {
  t0 <- trajectory() %>% seize("r1", 1)
  t1 <- trajectory() %>% seize("r2", 1)

  t2 <- trajectory() %>%
    select(c("r1", "r2", "r3"), policy = "round-robin") %>%
    seize_selected(1)

  env <- simmer(verbose = TRUE) %>%
    add_resource("r1", 2) %>%
    add_resource("r2", 3) %>%
    add_resource("r3", 1) %>%
    add_generator("dummy0", t0, at(0)) %>%
    add_generator("dummy1", t1, at(0, 0)) %>%
    add_generator("dummy2", t2, at(seq(1, 6)))

  expect_warning(env %>% run)

  res <- get_mon_resources(env)
  res_ordered <- res[order(res$time), ]
  res_ordered <- res_ordered[4:9, ]

  expect_equal(res_ordered$server, c(2, 3, 1, 2, 3, 1))
  expect_equal(res_ordered$queue, c(0, 0, 0, 1, 1, 1))
  expect_equal(res_ordered$resource, c("r1", "r2", "r3", "r1", "r2", "r3"))
})

test_that("core selection algorithms work: first-available", {
  t0 <- trajectory() %>% seize("r1", 1)
  t1 <- trajectory() %>% seize("r2", 1)

  t2 <- trajectory() %>%
    select(c("r1", "r2", "r3"), policy = "first-available") %>%
    seize_selected(1)

  env <- simmer(ver = T) %>%
    add_resource("r1", 2, 1) %>%
    add_resource("r2", 3, 1) %>%
    add_resource("r3", 1, 1) %>%
    add_generator("dummy0", t0, at(0)) %>%
    add_generator("dummy1", t1, at(0, 0)) %>%
    add_generator("dummy2", t2, at(seq(1, 7)))

  expect_warning(env %>% run)

  res <- get_mon_resources(env)
  res_ordered <- res[order(res$time), ]
  res_ordered <- res_ordered[4:9, ]

  expect_equal(res_ordered$server, c(2, 3, 1, 2, 3, 1))
  expect_equal(res_ordered$queue, c(0, 0, 0, 1, 1, 1))
  expect_equal(res_ordered$resource, c("r1", "r2", "r3", "r1", "r2", "r3"))
})

test_that("core selection algorithms work: random", {
  t0 <- trajectory() %>% seize("r1", 1)
  t1 <- trajectory() %>% seize("r2", 1)

  t2 <- trajectory() %>%
    select(c("r1", "r2", "r3"), policy = "random") %>%
    seize_selected(1)

  env <- simmer(verbose = TRUE) %>%
    add_resource("r1", 2) %>%
    add_resource("r2", 3) %>%
    add_resource("r3", 1) %>%
    add_generator("dummy0", t0, at(0)) %>%
    add_generator("dummy1", t1, at(0, 0)) %>%
    add_generator("dummy2", t2, at(seq(1, 6)))

  expect_warning(res1 <- get_mon_resources(env %>% reset %>% run))
  expect_warning(res2 <- get_mon_resources(env %>% reset %>% run))
  expect_warning(res3 <- get_mon_resources(env %>% reset %>% run))
  expect_true(!all(res1 == res2) || !all(res1 == res3))
})

test_that("custom selection algorithms work", {
  t0 <- trajectory() %>% seize("r1", 1)
  t1 <- trajectory() %>% seize("r2", 1)

  reverse_rr <- function() {
    res <- c("r1", "r2", "r3")
    i <- length(res) + 1
    function() {
      i <<- i - 1
      if (i == 0) i <<- length(res)
      return(res[[i]])
    }
  }

  t2 <- trajectory() %>%
    select(reverse_rr()) %>%
    seize_selected(1)

  env <- simmer(verbose = TRUE) %>%
    add_resource("r1", 2) %>%
    add_resource("r2", 3) %>%
    add_resource("r3", 1) %>%
    add_generator("dummy0", t0, at(0)) %>%
    add_generator("dummy1", t1, at(0, 0)) %>%
    add_generator("dummy2", t2, at(seq(1, 6)))

  expect_warning(env %>% run)

  res <- get_mon_resources(env)
  res_ordered <- res[order(res$time), ]
  res_ordered <- res_ordered[4:9, ]

  expect_equal(res_ordered$server, c(1, 3, 2, 1, 3, 2))
  expect_equal(res_ordered$queue, c(0, 0, 0, 1, 1, 1))
  expect_equal(res_ordered$resource, c("r3", "r2", "r1", "r3", "r2", "r1"))
})
