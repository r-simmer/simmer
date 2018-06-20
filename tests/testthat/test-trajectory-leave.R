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

context("leave")

test_that("an arrival leaves", {
  t0 <- trajectory() %>%
    seize("dummy", 1) %>%
    timeout(1) %>%
    leave(1) %>%
    timeout(1) %>%
    release("dummy", 1)

  t1 <- trajectory() %>%
    seize("dummy", 1) %>%
    timeout(1) %>%
    leave(function() 1) %>%
    timeout(1) %>%
    release("dummy", 1)

  env0 <- simmer(verbose = T) %>%
    add_resource("dummy") %>%
    add_generator("arrival", t0, at(0))

  env1 <- simmer(verbose = T) %>%
    add_resource("dummy") %>%
    add_generator("arrival", t1, at(0))

  expect_warning(run(env0))
  expect_warning(run(env1))

  arrivals0 <- get_mon_arrivals(env0)
  arrivals1 <- get_mon_arrivals(env1)

  expect_false(arrivals0$finished)
  expect_false(arrivals1$finished)
  expect_equal(arrivals0$activity_time, 1)
  expect_equal(arrivals1$activity_time, 1)
})

test_that("an arrival continues", {
  t0 <- trajectory() %>%
    seize("dummy", 1) %>%
    timeout(1) %>%
    leave(0) %>%
    timeout(1) %>%
    release("dummy", 1)

  t1 <- trajectory() %>%
    seize("dummy", 1) %>%
    timeout(1) %>%
    leave(function() 0) %>%
    timeout(1) %>%
    release("dummy", 1)

  arrivals0 <- simmer(verbose = T) %>%
    add_resource("dummy") %>%
    add_generator("arrival", t0, at(0)) %>%
    run() %>% get_mon_arrivals()

  arrivals1 <- simmer(verbose = T) %>%
    add_resource("dummy") %>%
    add_generator("arrival", t1, at(0)) %>%
    run() %>% get_mon_arrivals()

  expect_true(arrivals0$finished)
  expect_true(arrivals1$finished)
  expect_equal(arrivals0$activity_time, 2)
  expect_equal(arrivals1$activity_time, 2)
})
