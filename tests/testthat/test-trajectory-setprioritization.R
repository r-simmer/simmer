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

context("set_prioritization")

test_that("the wrong number of parameters fails", {
  expect_output(trajectory() %>% set_prioritization(c(0, 0, 0)) %>% print,
                ".*SetPrior.*values:.*0, 0, 0.*")

  t <- trajectory() %>%
    set_prioritization(function() c(0, 0))

  env <- simmer(verbose = TRUE) %>%
    add_generator("dummy", t, at(0))

  expect_error(env %>% run)
})

test_that("prioritization values change", {
  t0 <- trajectory() %>%
    set_prioritization(c(1, 2, 0)) %>%
    set_attribute(c("prio", "pree", "rest"), function() get_prioritization(env)) %>%
    set_prioritization(c(2, 3, 1), mod="+") %>%
    set_attribute(c("prio", "pree", "rest"), function() get_prioritization(env)) %>%
    set_prioritization(c(2, 3, 0), mod="*") %>%
    set_attribute(c("prio", "pree", "rest"), function() get_prioritization(env))

  env <- simmer(verbose = TRUE) %>%
    add_generator("dummy", t0, at(0), mon=2)

  attr <- env %>% run() %>% get_mon_attributes()

  expect_equal(attr$key, rep(c("prio", "pree", "rest"), 3))
  expect_equal(attr$value, c(1, 2, 0, 3, 5, 1, 6, 15, 0))
})

test_that("priority queues are adhered to (2)", {
  t0 <- trajectory() %>%
    seize("server", 1) %>%
    timeout(2) %>%
    release("server", 1)
  t1 <- trajectory() %>%
    set_prioritization(function() {
      prio <- get_prioritization(env)
      c(prio[[1]] + 1, 0, FALSE)
    }) %>%
    seize("server", 1) %>%
    timeout(2) %>%
    release("server", 1)

  env <- simmer(verbose = TRUE) %>%
    add_resource("server", 1) %>%
    add_generator("__nonprior", t0, at(c(0, 0))) %>%
    add_generator("__prior", t1, at(1)) # should be served second

  expect_warning(run(env))

  arrs <-
    env %>% get_mon_arrivals()

  expect_equal(arrs[arrs$name == "__prior0", ]$end_time, 4)
})

test_that("priority queues are adhered to (3)", {
  t0 <- trajectory() %>%
    seize("server", 1) %>%
    timeout(2) %>%
    release("server", 1)
  t1 <- trajectory() %>%
    set_prioritization(function() {
      prio <- get_prioritization(env)
      c(prio[[1]] + 1, 0, FALSE)
    }) %>%
    seize("server", 1) %>%
    timeout(2) %>%
    release("server", 1)

  env <- simmer(verbose = TRUE) %>%
    add_resource("server", 1) %>%
    add_generator("__nonprior", t0, at(c(0, 0))) %>%
    add_generator("__prior", t1, at(1))# should be served second

  expect_warning(run(env))

  arrs <-
    env %>% get_mon_arrivals()

  expect_equal(arrs[arrs$name == "__prior0", ]$end_time, 4)
})
