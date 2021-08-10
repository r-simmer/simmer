# Copyright (C) 2016-2018 Iñaki Ucar
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

test_that("capacity and queue size change as expected (1)", {
  t <- trajectory() %>%
    set_capacity("dummy", 0) %>%
    set_capacity("dummy", 1, mod="+") %>%
    set_capacity("dummy", 3, mod="*") %>%
    set_capacity("dummy", function() Inf) %>%
    set_capacity("dummy", 1, mod="+") %>%
    set_queue_size("dummy", Inf) %>%
    set_queue_size("dummy", 3, mod="*") %>%
    set_queue_size("dummy", function() 5) %>%
    set_queue_size("dummy", 5) %>%
    set_queue_size("dummy", 1, mod="+") %>%
    set_queue_size("dummy", 3, mod="*") %>%
    seize("dummy", 1)

  env <- simmer(verbose = TRUE) %>%
    add_resource("dummy", 5, 0) %>%
    add_generator("asdf", t, at(rep(0, 10)))

  expect_equal(env %>% get_capacity("dummy"), 5)
  expect_equal(env %>% get_queue_size("dummy"), 0)
  env %>% stepn(2)
  expect_equal(env %>% get_capacity("dummy"), 0)
  env %>% stepn()
  expect_equal(env %>% get_capacity("dummy"), 1)
  env %>% stepn()
  expect_equal(env %>% get_capacity("dummy"), 3)
  env %>% stepn()
  expect_equal(env %>% get_capacity("dummy"), Inf)
  env %>% stepn()
  expect_equal(env %>% get_capacity("dummy"), Inf)
  env %>% stepn()
  expect_equal(env %>% get_queue_size("dummy"), Inf)
  env %>% stepn()
  expect_equal(env %>% get_queue_size("dummy"), Inf)
  env %>% stepn()
  expect_equal(env %>% get_queue_size("dummy"), 5)
  env %>% stepn()
  expect_equal(env %>% get_queue_size("dummy"), 5)
  env %>% stepn()
  expect_equal(env %>% get_queue_size("dummy"), 6)
  env %>% stepn()
  expect_equal(env %>% get_queue_size("dummy"), 18)
  expect_warning(env %>% run)
  expect_equal(env %>% get_server_count("dummy"), 10)
})

test_that("capacity and queue size change as expected (2)", {
  t <- trajectory() %>%
    seize("dummy0", 2) %>%
    seize("dummy1", 3) %>%
    select("dummy0", id = 0) %>%
    select(function() "dummy1", id = 1) %>%
    set_capacity_selected(0, id = 0) %>%
    set_capacity_selected(function() Inf, id = 1) %>%
    set_queue_size_selected(Inf, id = 1) %>%
    set_queue_size_selected(function() 5, id = 0) %>%
    log_(function() paste(
      get_capacity_selected(env, id=0),
      get_server_count_selected(env, id=0),
      get_queue_size_selected(env, id=0),
      get_queue_count_selected(env, id=0),
      get_capacity_selected(env, id=1),
      get_server_count_selected(env, id=1),
      get_queue_size_selected(env, id=1),
      get_queue_count_selected(env, id=1)
    )) %>%
    release("dummy0", 2) %>%
    release("dummy1", 3)

  env <- simmer(verbose = TRUE) %>%
    add_resource("dummy0", 3, 0) %>%
    add_resource("dummy1", 3, 0) %>%
    add_generator("asdf", t, at(0))

  expect_equal(env %>% get_capacity("dummy0"), 3)
  expect_equal(env %>% get_capacity("dummy1"), 3)
  expect_equal(env %>% get_queue_size("dummy0"), 0)
  expect_equal(env %>% get_queue_size("dummy1"), 0)
  env %>% stepn(6)
  expect_equal(env %>% get_capacity("dummy0"), 0)
  env %>% stepn()
  expect_equal(env %>% get_capacity("dummy1"), Inf)
  env %>% stepn()
  expect_equal(env %>% get_queue_size("dummy1"), Inf)
  env %>% stepn()
  expect_equal(env %>% get_queue_size("dummy0"), 5)
  expect_output(run(env), "0 2 5 0 Inf 3 Inf 0")
})

preempt <- trajectory() %>% set_capacity("res", 0)
drop <- trajectory() %>% set_queue_size("res", 0)
t <- trajectory() %>%
  seize("res", 1) %>%
  timeout(10)

test_that("arrivals are preempted also when the last capacity was infinite", {
  env <- simmer(verbose = TRUE) %>%
    add_resource("res", Inf, preemptive = TRUE) %>%
    add_generator("dummy", t, at(0)) %>%
    add_generator("drop", preempt, at(1))

  expect_equal(run(env, 1) %>% get_server_count("res"), 1)
  expect_equal(run(env, 2) %>% get_server_count("res"), 0)
  expect_equal(get_queue_count(env, "res"), 1)
})

test_that("queue is not dropped without queue_size_strict (1)", {
  env <- simmer(verbose = TRUE) %>%
    add_resource("res", 0, preemptive = FALSE, queue_size_strict = FALSE) %>%
    add_generator("dummy", t, at(rep(0, 3))) %>%
    add_generator("drop", drop, at(1))

  expect_equal(run(env, 1) %>% get_queue_count("res"), 3)
  expect_equal(run(env, 2) %>% get_queue_count("res"), 3)
})

test_that("queue is not dropped without queue_size_strict (2)", {
  env <- simmer(verbose = TRUE) %>%
    add_resource("res", 0, preemptive = TRUE, queue_size_strict = FALSE) %>%
    add_generator("dummy", t, at(rep(0, 3))) %>%
    add_generator("drop", drop, at(1))

  expect_equal(run(env, 1) %>% get_queue_count("res"), 3)
  expect_equal(run(env, 2) %>% get_queue_count("res"), 3)
})

test_that("queue is dropped with queue_size_strict (1)", {
  env <- simmer(verbose = TRUE) %>%
    add_resource("res", 0, preemptive = FALSE, queue_size_strict = TRUE) %>%
    add_generator("dummy", t, at(rep(0, 3))) %>%
    add_generator("drop", drop, at(1))

  expect_equal(run(env, 1) %>% get_queue_count("res"), 3)
  expect_equal(run(env, 2) %>% get_queue_count("res"), 0)
})

test_that("queue is dropped with queue_size_strict (2)", {
  env <- simmer(verbose = TRUE) %>%
    add_resource("res", 0, preemptive = TRUE, queue_size_strict = TRUE) %>%
    add_generator("dummy", t, at(rep(0, 3))) %>%
    add_generator("drop", drop, at(1))

  expect_equal(run(env, 1) %>% get_queue_count("res"), 3)
  expect_equal(run(env, 2) %>% get_queue_count("res"), 0)
})

test_that("self-induced preemption works", {
  t <- trajectory() %>%
    seize("res") %>%
    set_capacity("res", 0) %>%
    timeout(10) %>%
    release("res")

  env <- simmer(verbose = TRUE) %>%
    add_resource("res", preemptive = TRUE) %>%
    add_generator("dummy", t, at(0)) %>%
    run(5)

  expect_equal(get_queue_count(env, "res"), 1)
  expect_equal(now(env), 0)
  expect_equal(peek(env), numeric(0))
})
