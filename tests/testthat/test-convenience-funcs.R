# Copyright (C) 2015-2019,2021 Iñaki Ucar
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

test_that("at returns the correct values", {
  gen_func <- at(c(0, 10, 15)) # values passed as vector
  expect_equal(gen_func(), c(0, 10, 5, -1))

  gen_func <- at(0, 10, 15) # values passed as parameters
  expect_equal(gen_func(), c(0, 10, 5, -1))
})

test_that("from returns the correct values", {
  gen_func <- from(5, function() 1)
  expect_equal(gen_func(), 5)
  expect_equal(gen_func(), 1)

  gen_func <- from(5, function() 1, arrive = FALSE)
  expect_equal(gen_func(), 6)
  expect_equal(gen_func(), 1)
})

test_that("to returns the correct values", {
  gen_func <- to(3, function() 1)
  expect_equal(gen_func(), 1)
  expect_equal(gen_func(), 1)
  expect_equal(gen_func(), -1)

  gen_func <- to(3, function() c(1, 1))
  expect_equal(gen_func(), c(1, 1))
  expect_equal(gen_func(), -1)

  gen_func <- to(3, function() c(1, 1, 1))
  expect_equal(gen_func(), c(1, 1, -1))
})

test_that("from_to returns the correct values", {
  gen_func <- from_to(5, 8, function() 1)
  expect_equal(gen_func(), 5)
  expect_equal(gen_func(), 1)
  expect_equal(gen_func(), 1)
  expect_equal(gen_func(), -1)

  gen_func <- from_to(5, 8, function() c(1, 1))
  expect_equal(gen_func(), 5)
  expect_equal(gen_func(), c(1, 1))
  expect_equal(gen_func(), -1)

  gen_func <- from_to(5, 8, function() c(1, 1, 1))
  expect_equal(gen_func(), 5)
  expect_equal(gen_func(), c(1, 1, -1))

  gen_func <- from_to(5, 8, function() 1, every=10)
  expect_equal(gen_func(), 5)
  expect_equal(gen_func(), 1)
  expect_equal(gen_func(), 1)
  expect_equal(gen_func(), 8)
  expect_equal(gen_func(), 1)
  expect_equal(gen_func(), 1)
  expect_equal(gen_func(), 8)

  gen_func <- from_to(5, 8, function() c(1, 1), every=10)
  expect_equal(gen_func(), 5)
  expect_equal(gen_func(), c(1, 1))
  expect_equal(gen_func(), 8)
  expect_equal(gen_func(), c(1, 1))
  expect_equal(gen_func(), 8)

  gen_func <- from_to(5, 8, function() c(1, 1, 1), every=10)
  expect_equal(gen_func(), 5)
  expect_equal(gen_func(), c(1, 1))
  expect_equal(gen_func(), 8)
  expect_equal(gen_func(), c(1, 1))
  expect_equal(gen_func(), 8)

  gen_func <- from_to(5, 8, function() 1, arrive = FALSE)
  expect_equal(gen_func(), 6)
  expect_equal(gen_func(), 1)
  expect_equal(gen_func(), -1)

  gen_func <- from_to(5, 8, function() c(1, 1), arrive = FALSE)
  expect_equal(gen_func(), c(6, 1))
  expect_equal(gen_func(), -1)

  gen_func <- from_to(5, 8, function() c(1, 1, 1), arrive = FALSE)
  expect_equal(gen_func(), c(6, 1, -1))

  gen_func <- from_to(5, 8, function() 1, arrive = FALSE, every=10)
  expect_equal(gen_func(), 6)
  expect_equal(gen_func(), 1)
  expect_equal(gen_func(), 9)
  expect_equal(gen_func(), 1)
  expect_equal(gen_func(), 9)

  gen_func <- from_to(5, 8, function() c(1, 1), arrive = FALSE, every=10)
  expect_equal(gen_func(), c(6, 1))
  expect_equal(gen_func(), c(9, 1))
  expect_equal(gen_func(), c(9, 1))

  gen_func <- from_to(5, 8, function() c(1, 1, 1), arrive = FALSE, every=10)
  expect_equal(gen_func(), c(6, 1))
  expect_equal(gen_func(), c(9, 1))
  expect_equal(gen_func(), c(9, 1))
})

test_that("environments are properly replaced and variables transferred", {
  start_time <- local({ start <- 1; function() start })
  stop_time <- local({ stop <- 2; function() stop })
  dist <- local({ x <- 3; function() x })
  every <- local({ ev <- 4; function() ev })

  gen_func <- from(start_time, dist)
  expect_equal(environment(gen_func)$start, 1)
  expect_equal(environment(gen_func)$stop, NULL)
  expect_equal(environment(gen_func)$x, 3)
  expect_equal(environment(gen_func)$ev, NULL)

  gen_func <- to(stop_time, dist)
  expect_equal(environment(gen_func)$start, NULL)
  expect_equal(environment(gen_func)$stop, 2)
  expect_equal(environment(gen_func)$x, 3)
  expect_equal(environment(gen_func)$ev, NULL)

  gen_func <- from_to(start_time, stop_time, dist, every=every)
  expect_equal(environment(gen_func)$start, 1)
  expect_equal(environment(gen_func)$stop, 2)
  expect_equal(environment(gen_func)$x, 3)
  expect_equal(environment(gen_func)$ev, 4)
})

test_that("schedule returns the correct values", {
  expect_error(schedule(1, 1))
  expect_error(schedule(c(1, 2), 1))
  expect_error(schedule(1, c(1, 2)))
  expect_error(schedule(c(2, 1), c(1, 2)))
  expect_error(schedule(c(2, 1), c(1, 2),  1))
  expect_error(schedule(c(0, 1), c(1, 2),  1))

  sch <- schedule(c(1, 3), c(1, 2), Inf)$schedule
  expect_equal(sch$init, 0)
  expect_equal(sch$intervals, c(1, 2))
  expect_equal(sch$values, c(1, 2))
  expect_equal(sch$period, -1)

  sch <- schedule(c(1, 3), c(1, 2), 3)$schedule
  expect_equal(sch$init, 2)
  expect_equal(sch$intervals, c(1, 2, 1))
  expect_equal(sch$values, c(1, 2, 1))
  expect_equal(sch$period, 3)

  sch <- schedule(c(0, 2), c(1, 2), 3)$schedule
  expect_equal(sch$init, 1)
  expect_equal(sch$intervals, c(0, 2, 1))
  expect_equal(sch$values, c(1, 2, 1))
  expect_equal(sch$period, 3)
})

test_that("when_activated returns the correct values", {
  gen_func <- when_activated()
  expect_equal(gen_func(), -1)
  expect_equal(gen_func(), c(0, -1))

  gen_func <- when_activated(function() 5)
  expect_equal(gen_func(), -1)
  expect_equal(gen_func(), c(rep(0, 5), -1))
})
