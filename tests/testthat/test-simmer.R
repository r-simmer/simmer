# Copyright (C) 2015-2016 Iñaki Ucar
# Copyright (C) 2016 Iñaki Ucar and Bart Smeets
# Copyright (C) 2016-2019 Iñaki Ucar
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

test_that("an empty environment behaves as expected", {
  output <- paste0(
    "simmer environment: SuperDuperSim | now: 0 | next: 0",
    ".*Monitor: in memory.*",
    ".*Resource: asdf | monitored: TRUE | server status: 0(1) | queue status: 0(Inf).*",
    ".*Source: dummy | monitored: 1 | n_generated: 0.*")

  env <- simmer("SuperDuperSim", verbose = TRUE) %>%
    add_resource("asdf") %>%
    add_generator("dummy", trajectory() %>% timeout(1), at(0))

  expect_output(print(env), output)

  expect_is(env, "simmer")
  expect_equal(env %>% now(), 0)
  expect_equal(env %>% peek(), 0)

  env %>% stepn() %>% run()

  expect_equal(env %>% now(), 1)
  expect_equal(env %>% peek(), numeric(0))
})

t0 <- trajectory("") %>%
  seize("server", 1) %>%
  set_attribute("dummy", 1) %>%
  timeout(1) %>%
  release("server", 1)

test_that("the simulator is reset (1)", {
  t1 <- trajectory() %>%
    seize("server", 1) %>%
    set_attribute("dummy", 1) %>%
    timeout(1) %>%
    release("server", 1)

  inf_sch <- schedule(c(0.5, 1), c(1, 1), Inf)

  env <- simmer(verbose = TRUE) %>%
    add_resource("server", inf_sch, queue_size = 1, preemptive = TRUE) %>%
    add_generator("entity0", t0, function() 0.5) %>%
    add_generator("entity1", t1, function() 0.5, mon = 2, preemptible = 10, priority = 10) %>%
    run(4) %>%
    reset()

  expect_equal(env %>% now(), 0)
  expect_equal(env %>% peek(), 0)
  expect_equal(nrow(get_mon_arrivals(env)), 0)
  expect_equal(nrow(get_mon_arrivals(env, TRUE)), 0)
  expect_equal(nrow(get_mon_resources(env)), 1)
  expect_equal(nrow(get_mon_attributes(env)), 0)
})

test_that("the simulator is reset (2)", {
  t1 <- trajectory() %>%
    renege_in(3) %>%
    seize("res") %>%
    renege_abort() %>%
    timeout(5) %>%
    release("res")

  env <- simmer(verbose = TRUE) %>%
    add_resource("res") %>%
    add_generator("dummy", t1, at(0, 0)) %>%
    run(2)

  expect_silent(reset(env))

  expect_equal(env %>% now(), 0)
  expect_equal(env %>% peek(), 0)
  expect_equal(nrow(get_mon_arrivals(env)), 0)
  expect_equal(nrow(get_mon_arrivals(env, TRUE)), 0)
  expect_equal(nrow(get_mon_resources(env)), 0)
  expect_equal(nrow(get_mon_attributes(env)), 0)
})

test_that("the progress is reported", {
  progress <- NULL
  record <- function(x) progress <<- c(progress, x)

  env <- simmer() %>%
    add_generator("dummy", trajectory(), at(0)) %>%
    run(progress=record)

  expect_equal(progress, seq(0, 1, 0.1))
})

test_that("the simulator stops if there are no more events", {
  env <- simmer(verbose = TRUE) %>%
    add_resource("server", 1) %>%
    add_generator("entity", t0, at(0)) %>%
    run(10)

  expect_equal(env %>% now(), 1)
})

test_that("a negative simulation time is converted to positive", {
  env <- simmer(verbose = TRUE) %>%
    add_resource("server", 1) %>%
    add_generator("entity", t0, at(10)) %>%
    run(-10)

  expect_equal(env %>% now(), 10)
})

test_that("a stopped simulation can be resumed", {
  env <- simmer(verbose = TRUE) %>%
    add_resource("server", 1) %>%
    add_generator("entity", t0, function() 1) %>%
    run(10)

  expect_equal(env %>% now(), 10)
  env %>% run(20)
  expect_equal(env %>% now(), 20)
  env %>% run(30)
  expect_equal(env %>% now(), 30)
})

test_that("there is verbose output", {
  output <- paste0(
    ".*(",
    ".*1.*arrival0.*Seize.*server",
    ".*1.*arrival0.*Release.*server",
    ").*")

  expect_output(
    env <- simmer(verbose = TRUE) %>%
      add_resource("server", 1) %>%
      add_generator("arrival", t0, at(1)) %>%
      run(),
    output
  )
})

test_that("we can force some errors (just to complete coverage)", {
  expect_error(simmer(0))
  expect_error(simmer(verbose = TRUE) %>% add_resource(0))

  env <- simmer(verbose = TRUE) %>%
    add_resource("dummy") %>%
    add_generator("dummy", trajectory(), function() 1, mon = 1000)
  env$sim_obj <- NULL
  env$mon$xptr <- NULL

  expect_error(env %>% reset())
  expect_error(env %>% now())
  expect_error(env %>% peek())
  expect_error(env %>% stepn())
  expect_error(env %>% get_mon_arrivals(FALSE))
  expect_error(env %>% get_mon_arrivals(TRUE))
  expect_error(env %>% get_mon_attributes())
  expect_error(env %>% get_mon_resources())

  sch <- schedule(c(1, 2), c(1, 2), Inf)
  sch$schedule$period <- "asdf"
  expect_error(simmer(verbose = TRUE) %>% add_resource("dummy", sch))

  env <- simmer(verbose = TRUE)
  expect_equal(env %>% get_mon_resources() %>% nrow(), 0)
})
