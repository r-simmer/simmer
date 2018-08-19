# Copyright (C) 2016,2018 Iñaki Ucar
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

context("resource-schedule")

test_that("a schedule name conflicts with a generator name", {
  env <- simmer(verbose = TRUE) %>%
    add_generator("asdf", trajectory(), at(0)) %>%
    add_generator("fdsa_capacity", trajectory(), at(0)) %>%
    add_generator("fdsa_queue_size", trajectory(), at(0))

  expect_error(env %>%
    add_resource("fdsa", schedule(c(1, 2), c(1, 1))))
  expect_error(env %>%
    add_resource("fdsa", queue_size=schedule(c(1, 2), c(1, 1))))
  expect_silent(env %>%
    add_resource("asdf", schedule(c(1, 2), c(1, 1))))
  expect_warning(env %>%
    add_generator("asdf_capacity", trajectory(), at(0)))
})

test_that("a schedule cannot be created if the corresponding resource doesn't exist", {
  ptr <- simmer()$.__enclos_env__$private$sim_obj
  expect_error(add_resource_manager_(ptr, "name", "capacity", c(0, 1), c(0, 1), -1))
})

test_that("capacity & queue size change", {
  inf_sch <- schedule(c(8, 16, 24), c(1, 2, 3), Inf)
  fin_sch <- schedule(c(8, 16, 24), c(1, 2, 3), 24)

  expect_output(print(inf_sch))

  limits <- simmer(verbose = TRUE) %>%
    add_resource("dummy", inf_sch) %>%
    run(17) %>% reset() %>% run(49) %>%
    get_mon_resources()

  expect_equal(limits$time, c(8, 16, 24))
  expect_equal(limits$capacity, c(1, 2, 3))

  limits <- simmer(verbose = TRUE) %>%
    add_resource("dummy", fin_sch) %>%
    run(17) %>% reset() %>% run(49) %>%
    get_mon_resources()

  expect_equal(limits$time, c(8, 16, 24, 32, 40, 48))
  expect_equal(limits$capacity, c(1, 2, 3, 1, 2, 3))
})

test_that("queue size changes", {
  inf_sch <- schedule(c(8, 16, 24), c(1, 2, 3), Inf)
  fin_sch <- schedule(c(8, 16, 24), c(1, 2, 3), 24)

  limits <- simmer(verbose = TRUE) %>%
    add_resource("dummy", 1, inf_sch) %>%
    run(17) %>% reset() %>% run(49) %>%
    get_mon_resources()

  expect_equal(limits$time, c(8, 16, 24))
  expect_equal(limits$queue_size, c(1, 2, 3))

  limits <- simmer(verbose = TRUE) %>%
    add_resource("dummy", 1, fin_sch) %>%
    run(17) %>% reset() %>% run(49) %>%
    get_mon_resources()

  expect_equal(limits$time, c(8, 16, 24, 32, 40, 48))
  expect_equal(limits$queue_size, c(1, 2, 3, 1, 2, 3))
})

test_that("arrivals 1) are dequeued when resource's capacity increases and
                    2) remain in server when it decreases", {
  t <- trajectory() %>%
    seize("dummy", 1) %>%
    timeout(2) %>%
    release("dummy", 1)

  inf_sch <- schedule(c(0, 1, 2), c(1, 3, 1), Inf)

  arrivals <- simmer(verbose = TRUE) %>%
    add_resource("dummy", inf_sch) %>%
    add_generator("asdf", t, at(0, 0, 0)) %>%
    run() %>%
    get_mon_arrivals()

  expect_equal(arrivals$end_time, c(2, 3, 3))
  expect_equal(arrivals$activity_time, c(2, 2, 2))
})

test_that("arrivals 1) are dequeued when resource's capacity increases and
                    2) remain in server when it decreases", {
  t <- trajectory() %>%
    seize("dummy", 1) %>%
    timeout(2) %>%
    release("dummy", 1)

  inf_sch <- schedule(c(0, 1, 2), c(1, 3, 1), Inf)

  arrivals <- simmer(verbose = TRUE) %>%
    add_resource("dummy", inf_sch) %>%
    add_generator("asdf", t, at(0, 0, 0)) %>%
    run() %>%
    get_mon_arrivals()

  expect_equal(arrivals$end_time, c(2, 3, 3))
  expect_equal(arrivals$activity_time, c(2, 2, 2))
})

test_that("arrivals are preempted when resource's capacity decreases", {
  t <- trajectory() %>%
    seize("dummy", 1) %>%
    timeout(2) %>%
    release("dummy", 1)

  inf_sch <- schedule(c(0, 1, 2), c(1, 3, 1), Inf)

  arrivals <- simmer(verbose = TRUE) %>%
    add_resource("dummy", inf_sch, preemptive = TRUE) %>%
    add_generator("asdf", t, at(0, 0, 0), restart = TRUE) %>%
    run() %>%
    get_mon_arrivals()

  expect_equal(arrivals$end_time, c(2, 3, 5))
  expect_equal(arrivals$activity_time, c(2, 2, 3))
})

test_that("resource's capacity decreases before post-release tasks", {
  t <- trajectory() %>%
    seize("t-rex") %>%
    timeout(5) %>%
    release("t-rex")

  arrivals <- simmer(verbose = TRUE) %>%
    add_resource("t-rex", capacity = schedule(timetable = c(5, 10, 15),
                                              period = Inf,
                                              values = c(1, 0, 1))) %>%
    add_generator("piggy", t, at(0, 0, 0)) %>%
    run() %>%
    get_mon_arrivals()

  expect_equal(arrivals$end_time, c(10, 20, 25))
  expect_equal(arrivals$activity_time, c(5, 5, 5))
})

test_that("capacity decrease on a non-released preemptive resource does not crash", {
  t <- trajectory() %>%
    seize("dummy", 1)

  sched <- schedule(c(0, 1), c(1, 0), period = Inf)

  env <- simmer(verbose = TRUE) %>%
    add_resource("dummy", sched, preemptive = TRUE) %>%
    add_generator("arrival", t, at(0))

  expect_warning(run(env))
})
