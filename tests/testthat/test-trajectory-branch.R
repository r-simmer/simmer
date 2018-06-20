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

context("branch")

test_that("a non-function option fails", {
  expect_error(trajectory() %>% branch(1, TRUE, trajectory()))
})

test_that("the wrong number of elements fails, but continue is recycled", {
  expect_error(trajectory() %>% branch(function() 1, c(TRUE, TRUE), trajectory()))
  expect_silent(trajectory() %>% branch(function() 1, TRUE, trajectory(), trajectory()))
})

test_that("an index equal to 0 skips the branch", {
  t0 <- trajectory() %>%
    branch(function() 0, TRUE,
           trajectory() %>% timeout(1)
    ) %>%
    timeout(2)

  env <- simmer(verbose = TRUE) %>%
    add_generator("entity", t0, at(0)) %>%
    run()
  expect_equal(env %>% now(), 2)
})

test_that("an index out of range fails", {
  t1 <- trajectory() %>%
    branch(function() 1, TRUE,
      trajectory() %>% timeout(1)
    )
  t2 <- trajectory() %>%
    branch(function() 2, TRUE,
      trajectory() %>% timeout(1)
    )

  env <- simmer(verbose = TRUE) %>%
    add_generator("entity", t2, at(0))
  expect_error(env %>% run())

  env <- simmer(verbose = TRUE) %>%
    add_generator("entity", t1, at(0)) %>%
    run()
  expect_equal(env %>% now(), 1)
})

test_that("accepts a list of trajectories", {
  t1 <- trajectory() %>% timeout(1)

  t2 <- trajectory() %>%
    branch(function() 1, continue=TRUE, replicate(10, t1))

  expect_equal(length(t2), 1)
  expect_equal(get_n_activities(t2), 11)
})

test_that("the continue argument works as expected", {
  sequential <- function(n) {
    i <- 0
    function() {
      i <<- i + 1
      if (i > n) i <<- 1
      i
    }
  }

  t0 <- trajectory()
  t1 <- trajectory() %>% timeout(1)

  t <- trajectory() %>%
    branch(
      sequential(4),
      continue=c(FALSE, TRUE, FALSE, TRUE),
      t0, t0, t1, t1
    ) %>%
    timeout(1)

  arr <- simmer(verbose = TRUE) %>%
    add_generator("dummy", t, at(0:3)) %>%
    run() %>%
    get_mon_arrivals()

  expect_equal(arr$start_time, 0:3)
  expect_equal(arr$activity_time, c(0, 1, 1, 2))
})
