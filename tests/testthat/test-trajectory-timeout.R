context("timeout")

test_that("incorrect types fail", {
  expect_error(trajectory() %>% timeout("dummy"))
  expect_error(trajectory() %>% timeout_from_attribute(2))
})

test_that("a task function that returns a non-numeric value fails", {
  t0 <- trajectory() %>%
    timeout(function() {})

  env <- simmer(verbose = TRUE) %>%
    add_generator("entity", t0, function() 1)

  expect_error(run(env))
})

test_that("a missing value fails", {
  t0 <- trajectory() %>%
    timeout(NaN)
  t1 <- trajectory() %>%
    timeout_from_attribute("asdf")

  env0 <- simmer(verbose = TRUE) %>%
    add_generator("dummy", t0, at(0))
  env1 <- simmer(verbose = TRUE) %>%
    add_generator("dummy", t1, at(0))

  expect_error(trajectory() %>% timeout(NA))
  expect_error(run(env0))
  expect_error(run(env1))
})

test_that("a timeout is correctly monitored", {
  t <- trajectory() %>%
    set_attribute("three", 3) %>%
    set_global("minusthree", -3) %>%
    seize("dummy") %>%
    timeout(-3) %>%
    timeout(3) %>%
    timeout(function() 4) %>%
    timeout_from_attribute("three") %>%
    timeout_from_global("minusthree") %>%
    release("dummy")

  env <- simmer(verbose = TRUE) %>%
    add_generator("entity", t, at(0)) %>%
    add_resource("dummy") %>%
    run()

  expect_equal(get_mon_arrivals(env)[1, ]$end_time, 16)
  expect_equal(get_mon_arrivals(env, TRUE)[1, ]$end_time, 16)
})

test_that("an infinite timeout can be defined", {
  t <- trajectory() %>%
    timeout(Inf)

  arr <- simmer() %>%
    add_generator("dummy", t, at(0)) %>%
    run() %>%
    get_mon_arrivals()

  expect_equal(arr$end_time, Inf)
})
