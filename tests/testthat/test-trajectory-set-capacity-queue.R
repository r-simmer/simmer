context("set_capacity/set_queue_size")

test_that("capacity and queue size change as expected (1)", {
  t <- trajectory() %>%
    set_capacity("dummy", 0) %>%
    set_capacity("dummy", function() Inf) %>%
    set_queue_size("dummy", Inf) %>%
    set_queue_size("dummy", function() 5) %>%
    set_queue_size("dummy", 5) %>%
    seize("dummy", 1)

  env <- simmer(verbose = TRUE) %>%
    add_resource("dummy", 5, 0) %>%
    add_generator("asdf", t, at(rep(0, 10)))

  expect_equal(env %>% get_capacity("dummy"), 5)
  expect_equal(env %>% get_queue_size("dummy"), 0)
  env %>% onestep() %>% onestep()
  expect_equal(env %>% get_capacity("dummy"), 0)
  env %>% onestep()
  expect_equal(env %>% get_capacity("dummy"), Inf)
  env %>% onestep()
  expect_equal(env %>% get_queue_size("dummy"), Inf)
  env %>% onestep()
  expect_equal(env %>% get_queue_size("dummy"), 5)
  expect_warning(env %>% run)
  expect_equal(env %>% get_server_count("dummy"), 10)
})

test_that("capacity and queue size change as expected (2)", {
  t <- trajectory() %>%
    select("dummy0", id = 0) %>%
    select(function() "dummy1", id = 1) %>%
    set_capacity_selected(0, id = 0) %>%
    set_capacity_selected(function() Inf, id = 1) %>%
    set_queue_size_selected(Inf, id = 1) %>%
    set_queue_size_selected(function() 5, id = 0)

  expect_output(print(t))

  env <- simmer(verbose = TRUE) %>%
    add_resource("dummy0", 3, 0) %>%
    add_resource("dummy1", 3, 0) %>%
    add_generator("asdf", t, at(0))

  expect_equal(env %>% get_capacity("dummy0"), 3)
  expect_equal(env %>% get_capacity("dummy1"), 3)
  expect_equal(env %>% get_queue_size("dummy0"), 0)
  expect_equal(env %>% get_queue_size("dummy1"), 0)
  env %>% onestep() %>% onestep() %>% onestep() %>% onestep()
  expect_equal(env %>% get_capacity("dummy0"), 0)
  env %>% onestep()
  expect_equal(env %>% get_capacity("dummy1"), Inf)
  env %>% onestep()
  expect_equal(env %>% get_queue_size("dummy1"), Inf)
  env %>% onestep()
  expect_equal(env %>% get_queue_size("dummy0"), 5)
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
