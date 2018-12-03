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

context("stop_if")

test_that("execution is stopped with a warning and can be continued", {
  t <- trajectory() %>%
    stop_if(function() FALSE) %>%
    timeout(1) %>%
    stop_if(TRUE) %>%
    timeout(1)

  env <- simmer(verbose = TRUE) %>%
    add_generator("dummy", t, at(0))

  expect_warning(run(env))
  expect_equal(now(env), 1)
  run(env)
  expect_equal(now(env), 2)
  expect_equal(get_mon_arrivals(env)$activity_time, 2)
})
