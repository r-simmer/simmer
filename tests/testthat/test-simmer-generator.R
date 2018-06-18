context("generator")

test_that("a generator without a trajectory fails", {
  expect_error(
    simmer(verbose = TRUE) %>%
      add_generator("customer", 4, 1))
})

test_that("a non-function dist fails", {
  expect_error(
    simmer(verbose = TRUE) %>%
      add_generator("customer", trajectory(), 1))
})

test_that("a dist that returns a non-numeric value fails", {
  expect_error(
    simmer(verbose = TRUE) %>%
      add_generator("customer", trajectory(), function() {}) %>%
      step())
})

test_that("generates the expected amount", {
  env <- simmer(verbose = TRUE) %>%
    add_generator("customer", trajectory(), at(1:3)) %>%
    run()
  arr <- get_mon_arrivals(env)

  expect_error(env %>% get_n_generated("asdf"))
  expect_equal(env %>% get_n_generated("customer"), 3)
  expect_equal(arr$start_time, 1:3)
  expect_equal(arr$end_time, 1:3)
  expect_equal(arr$activity_time, rep(0, 3))
})

test_that("generators are reset", {
  expect_equal(3, simmer(verbose = TRUE) %>%
    add_generator("dummy", trajectory(), at(0, 1, 2)) %>%
    run() %>% reset() %>% run() %>%
    get_mon_arrivals() %>% nrow()
  )
})

test_that("preemptible < priority shows a warning", {
  expect_warning(
    simmer(verbose = TRUE) %>%
      add_generator("dummy", trajectory(), at(0), priority = 3, preemptible = 1))
})

test_that("arrival names are correctly retrieved", {
  t <- trajectory() %>%
    log_(function() get_name(env))

  env <- simmer() %>%
    add_generator("dummy", t, at(0))

  expect_output(run(env), "0: dummy0: dummy0")
  expect_error(get_name(env))
})

test_that("arrivals are correctly monitored", {
  a <- trajectory() %>%
    seize("res2", 1) %>%
    batch(1) %>%
    seize("res1", 1) %>%
    timeout(5) %>%
    release("res1", 1) %>%
    separate() %>%
    release("res2", 1)

  b <- trajectory() %>%
    seize("res1", 1) %>%
    timeout(6) %>%
    release("res1", 1)

  c <- trajectory() %>%
    seize("res1", 1) %>%
    timeout(1) %>%
    rollback(1, times = Inf)

  env <- simmer(verbose = TRUE) %>%
    add_resource("res1", 1) %>%
    add_resource("res2") %>%
    add_generator("a", a, at(0)) %>%
    add_generator("b", b, at(0)) %>%
    add_generator("c", c, at(0)) %>%
    add_generator("d", c, at(1), mon = FALSE) %>%
    run(until = 4)

  arr1 <- get_mon_arrivals(env, per_resource = FALSE, ongoing = TRUE)
  arr1 <- arr1[order(arr1$name), ]
  arr2 <- get_mon_arrivals(env, per_resource = TRUE, ongoing = TRUE)
  arr2 <- arr2[order(arr2$name, arr2$resource), ]

  expect_equal(arr1$name, c("a0", "b0", "c0"))
  expect_equal(arr1$start_time, c(0, 0, 0))
  expect_equal(arr1$end_time, c(NA_real_, NA, NA))
  expect_equal(arr1$activity_time, c(NA_real_, NA, NA))
  expect_equal(arr1$finished, rep(FALSE, 3))
  expect_equal(arr2$name, c("a0", "a0", "b0", "c0"))
  expect_equal(arr2$start_time, c(0, 0, 0, 0))
  expect_equal(arr2$end_time, c(NA_real_, NA, NA, NA))
  expect_equal(arr2$activity_time, c(NA_real_, NA, NA, NA))
  expect_equal(arr2$resource, c("res1", "res2", "res1", "res1"))

  env %>%
    reset() %>%
    run(until = 10)

  arr1 <- get_mon_arrivals(env, per_resource = FALSE, ongoing = TRUE)
  arr1 <- arr1[order(arr1$name), ]
  arr2 <- get_mon_arrivals(env, per_resource = TRUE, ongoing = TRUE)
  arr2 <- arr2[order(arr2$name, arr2$resource), ]

  expect_equal(arr1$name, c("a0", "b0", "c0"))
  expect_equal(arr1$start_time, c(0, 0, 0))
  expect_equal(arr1$end_time, c(5, NA, NA))
  expect_equal(arr1$activity_time, c(5, NA, NA))
  expect_equal(arr1$finished, c(TRUE, FALSE, FALSE))
  expect_equal(arr2$name, c("a0", "a0", "b0", "c0"))
  expect_equal(arr2$start_time, c(0, 0, 0, 0))
  expect_equal(arr2$end_time, c(5, 5, NA, NA))
  expect_equal(arr2$activity_time, c(5, 5, NA, NA))
  expect_equal(arr2$resource, c("res1", "res2", "res1", "res1"))

  env %>%
    reset() %>%
    run(until = 12)

  arr1 <- get_mon_arrivals(env, per_resource = FALSE, ongoing = TRUE)
  arr1 <- arr1[order(arr1$name), ]
  arr2 <- get_mon_arrivals(env, per_resource = TRUE, ongoing = TRUE)
  arr2 <- arr2[order(arr2$name, arr2$resource), ]

  expect_equal(arr1$name, c("a0", "b0", "c0"))
  expect_equal(arr1$start_time, c(0, 0, 0))
  expect_equal(arr1$end_time, c(5, 11, NA))
  expect_equal(arr1$activity_time, c(5, 6, NA))
  expect_equal(arr1$finished, c(TRUE, TRUE, FALSE))
  expect_equal(arr2$name, c("a0", "a0", "b0", "c0"))
  expect_equal(arr2$start_time, c(0, 0, 0, 0))
  expect_equal(arr2$end_time, c(5, 5, 11, NA))
  expect_equal(arr2$activity_time, c(5, 5, 6, NA))
  expect_equal(arr2$resource, c("res1", "res2", "res1", "res1"))
})
