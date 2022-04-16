# Copyright (C) 2022 Iñaki Ucar
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

test_that("schedule checks work as expected", {
  # all numeric
  expect_error(schedule(c("1", "2"), c(1, 2), 2))
  expect_error(schedule(c(1, 2), c("1", "2"), 2))
  expect_error(schedule(c(1, 2), c(1, 2), "2"))
  # timetable is sorted
  expect_error(schedule(c(2, 1), c(1, 2), 2))
  # period >= timetable
  expect_error(schedule(c(1, 2), c(1, 2), 1))
  # start and end of period are not defined simultaneously
  expect_error(schedule(c(0, 2), c(1, 2), 2))
  # same length
  expect_error(schedule(c(1, 2), c(1, 2, 3), 2))
  # at least two values
  expect_error(schedule(c(1), c(1), 2))
  # non-negative
  expect_error(schedule(c(1, 2), c(-1, 2), 2))
})

test_that("schedules print the correct period", {
  x <- schedule(c(1, 2), c(1, 2))
  expect_s3_class(x, "schedule")
  expect_output(print(x), "Inf")

  x <- schedule(c(1, 2), c(1, 2), 10)
  expect_s3_class(x, "schedule")
  expect_output(print(x), "10")
})

test_that("schedule arithmetic works as expected", {
  x <- schedule(c(0, 5), c(1, 0), 24)
  y <- schedule(c(0, 4), c(0, 1))
  expect_error(x + y)
  expect_error(x + 1)
  expect_error(1 + x)

  x <- schedule(c(0, 5), c(1, 0))
  y <- schedule(c(0, 4), c(0, 1))
  z <- schedule(c(0, 4, 5), c(1, 2, 1))
  expect_equal(x + y, z)

  x <- schedule(c(8, 16, 24), c(3, 2, 1), 24)
  y <- schedule(c(1, 17, 23), c(5, 0, 3), 24)
  z <- schedule(c(1, 8, 16, 17, 23, 24), c(5, 8, 7, 2, 5, 4), 24)
  expect_equal(x + y, z)

  x <- schedule(c(1, 3, 5), c(1, 3, 5), period=5)
  y <- schedule(c(1, 3), c(1, 3), period=3)
  z <- schedule(c(1, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 15),
                c(2, 6, 4, 6, 4, 2, 4, 6, 6, 2, 4, 4, 8), 15)
  expect_equal(x + y, z)
})
