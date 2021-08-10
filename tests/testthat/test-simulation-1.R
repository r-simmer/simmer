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

test_that("a simple deterministic simulation with rejections behaves as expected", {
  n <- 100

  t0 <- trajectory("") %>%
    seize("server", 1) %>%
    timeout(function() 1.5) %>%
    release("server", 1)

  env <- simmer(verbose = TRUE) %>%
    add_resource("server", 1, queue_size = 0) %>%
    add_generator("entity", t0, at(1:n))

  expect_warning(env %>% add_generator("entity", t0, function() 2))
  expect_equal(env %>% now(), 0)
  expect_equal(env %>% peek(), 0)

  env %>% run(1.5)

  expect_equal(env %>% now(), 1.5)
  expect_equal(env %>% peek(), 2)

  env %>% run()
  arrivals <- env %>% get_mon_arrivals()
  arrivals_res <- env %>% get_mon_arrivals(TRUE)
  resources <- env %>% get_mon_resources()

  expect_equal(env %>% now(), n + 0.5)
  expect_equal(env %>% peek(), numeric(0))
  expect_equal(nrow(arrivals), n)
  expect_equal(nrow(arrivals_res), n / 2)
  expect_true(arrivals_res[1, ]$resource == "server")
  expect_equal(nrow(subset(arrivals, finished)), n / 2)
  expect_equal(nrow(subset(arrivals, !finished)), n / 2)
  expect_equal(sum(subset(arrivals, finished)$activity_time), 1.5 * n / 2)
  expect_equal(sum(arrivals_res$activity_time), 1.5 * n / 2)
  expect_equal(sum(subset(arrivals, !finished)$activity_time), 0)

  expect_equal(nrow(resources), n)
  expect_equal(sum(resources$server), n / 2)
  expect_equal(sum(resources$queue), 0)
})
