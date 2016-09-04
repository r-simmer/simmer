context("timeout")

test_that("a task function that returns a non-numeric value fails", {
  t0 <- create_trajectory() %>%
    timeout(function() {})

  env <- simmer() %>%
    add_generator("entity", t0, function() 1)

  expect_error(env %>% run(100))
})

test_that("a timeout is correctly monitored", {
  t <- create_trajectory() %>%
    seize("dummy") %>%
    timeout(-3) %>%
    timeout(3) %>%
    timeout(function() 4) %>%
    release("dummy")

  env <- simmer(verbose = TRUE) %>%
    add_generator("entity", t, at(0)) %>%
    add_resource("dummy") %>%
    run()

  expect_equal(get_mon_arrivals(env)[1, ]$end_time, 10)
  expect_equal(get_mon_arrivals(env, TRUE)[1, ]$end_time, 10)
})

test_that("incorrect types fail", {
  expect_error(create_trajectory() %>% timeout("dummy"))
})
