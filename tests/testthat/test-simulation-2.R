# Copyright (C) 2015-2016 Iñaki Ucar
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

context("simulation 2")

test_that("a release is executed before a seize in the same instant", {
  t0 <- trajectory() %>%
    seize("server", 1) %>%
    timeout(1) %>%
    release("server", 1)

  env <- simmer(verbose = TRUE) %>%
    add_resource("server", 1, 0) %>%
    add_generator("dummy1", t0, at(c(1, 3, 5, 7, 9))) %>%
    add_generator("dummy0", t0, at(c(0, 2, 4, 6, 8))) %>%
    run()

  arrivals <- env %>% get_mon_arrivals()

  expect_equal(sum(arrivals$finished), nrow(arrivals))
})
