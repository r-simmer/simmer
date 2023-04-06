# Copyright (C) 2016-2023 Iñaki Ucar
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

test_that("several deactivates don't crash", {
  t <- trajectory() %>%
    deactivate("dummy0")

  env <- simmer(verbose = env_verbose) %>%
    add_generator("dummy0", t, at(0, 2)) %>%
    add_generator("dummy1", t, at(1)) %>%
    run()

  expect_equal(now(env), 1)
})

test_that("generators are deactivated and activated again as expected", {
  t <- trajectory() %>%
    deactivate("dummy") %>%
    timeout(1) %>%
    activate("dummy")

  env <- simmer(verbose = env_verbose) %>%
    add_generator("dummy", t, function() 1) %>%
    run(10)
  arr <- get_mon_arrivals(env)

  expect_equal(arr$name, paste0("dummy", 0:3))
  expect_equal(arr$start_time, c(1, 3, 5, 7))
  expect_equal(arr$end_time, c(2, 4, 6, 8))
})
