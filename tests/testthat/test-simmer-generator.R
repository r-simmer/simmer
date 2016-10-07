context("generator")

test_that("a generator without a trajectory fails", {
  expect_error(simmer(verbose = TRUE) %>% add_generator("customer", 4, 1))
})

test_that("a non-function dist fails", {
  t0 <- create_trajectory()

  expect_error(simmer(verbose = TRUE) %>% add_generator("customer", t0, 1))
})

test_that("an empty trajectory fails", {
  t0 <- create_trajectory()

  expect_error(simmer(verbose = TRUE) %>% add_generator("customer", t0, function() {}))
})

test_that("a dist that returns a non-numeric value fails", {
  t0 <- create_trajectory() %>% timeout(1)

  expect_error(simmer(verbose = TRUE) %>% add_generator("customer", t0, function() {}))
})

test_that("generates the expected amount", {
  t0 <- create_trajectory() %>% timeout(1)

  env <- simmer(verbose = TRUE) %>%
    add_generator("customer", t0, at(c(0, 1, 2))) %>%
    run(10)

  expect_error(env %>% get_n_generated("asdf"))
  expect_equal(env %>% get_n_generated("customer"), 3)
})

test_that("generators are reset", {
  t <- create_trajectory() %>% timeout(1)

  expect_equal(3, simmer(verbose = TRUE) %>%
    add_generator("dummy", t, at(0, 1, 2)) %>%
    run() %>% reset() %>% run() %>%
    get_mon_arrivals() %>% nrow()
  )
})

test_that("preemptible < priority shows a warning", {
  t <- create_trajectory() %>% timeout(0)
  expect_warning(simmer(verbose = TRUE) %>% add_generator("dummy", t, at(0), priority = 3, preemptible = 1))
})

test_that("arrivals are correctly monitored", {
  a <- create_trajectory() %>%
    seize("res2", 1) %>%
    batch(1) %>%
    seize("res1", 1) %>%
    timeout(5) %>%
    release("res1", 1) %>%
    separate() %>%
    release("res2", 1)

  b <- create_trajectory() %>%
    seize("res1", 1) %>%
    timeout(6) %>%
    release("res1", 1)

  c <- create_trajectory() %>%
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
  expect_equal(arr1$finished, c(NA, NA, NA))
  expect_equal(arr2$name, c("a0", "a0", "b0", "c0"))
  expect_equal(arr2$start_time, c(0, 0, 0, 0))
  expect_equal(arr2$end_time, c(NA_real_, NA, NA, NA))
  expect_equal(arr2$activity_time, c(NA_real_, NA, NA, NA))
  expect_equal(arr2$resource, c("res1", "res2", "res1", "res1"))

  env %>% run(until = 10)

  arr1 <- get_mon_arrivals(env, per_resource = FALSE, ongoing = TRUE)
  arr1 <- arr1[order(arr1$name), ]
  arr2 <- get_mon_arrivals(env, per_resource = TRUE, ongoing = TRUE)
  arr2 <- arr2[order(arr2$name, arr2$resource), ]

  expect_equal(arr1$name, c("a0", "b0", "c0"))
  expect_equal(arr1$start_time, c(0, 0, 0))
  expect_equal(arr1$end_time, c(5, NA, NA))
  expect_equal(arr1$activity_time, c(5, NA, NA))
  expect_equal(arr1$finished, c(TRUE, NA, NA))
  expect_equal(arr2$name, c("a0", "a0", "b0", "c0"))
  expect_equal(arr2$start_time, c(0, 0, 0, 0))
  expect_equal(arr2$end_time, c(5, 5, NA, NA))
  expect_equal(arr2$activity_time, c(5, 5, NA, NA))
  expect_equal(arr2$resource, c("res1", "res2", "res1", "res1"))

  env %>% run(until = 12)

  arr1 <- get_mon_arrivals(env, per_resource = FALSE, ongoing = TRUE)
  arr1 <- arr1[order(arr1$name), ]
  arr2 <- get_mon_arrivals(env, per_resource = TRUE, ongoing = TRUE)
  arr2 <- arr2[order(arr2$name, arr2$resource), ]

  expect_equal(arr1$name, c("a0", "b0", "c0"))
  expect_equal(arr1$start_time, c(0, 0, 0))
  expect_equal(arr1$end_time, c(5, 11, NA))
  expect_equal(arr1$activity_time, c(5, 6, NA))
  expect_equal(arr1$finished, c(TRUE, TRUE, NA))
  expect_equal(arr2$name, c("a0", "a0", "b0", "c0"))
  expect_equal(arr2$start_time, c(0, 0, 0, 0))
  expect_equal(arr2$end_time, c(5, 5, 11, NA))
  expect_equal(arr2$activity_time, c(5, 5, 6, NA))
  expect_equal(arr2$resource, c("res1", "res2", "res1", "res1"))
})
