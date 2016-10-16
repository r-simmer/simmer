context("set_trajectory/set_distribution")

test_that("we can set a new trajectory", {
  t2 <- create_trajectory() %>%
    timeout(2)
  t1 <- create_trajectory() %>%
    set_trajectory("dummy", t2) %>%
    timeout(1)

  env <- simmer(verbose = TRUE) %>%
    add_generator("dummy", t1, function() 1) %>%
    run(10)
  arr <- get_mon_arrivals(env)

  expect_equal(arr$start_time, c(1, 2, 3, 4, 5, 6, 7))
  expect_equal(arr$end_time, c(2, 4, 5, 6, 7, 8, 9))
})

test_that("we can set a new distribution", {
  t <- create_trajectory() %>%
    set_distribution("dummy", function() 2)

  env <- simmer(verbose = TRUE) %>%
    add_generator("dummy", t, function() 1) %>%
    run(10)
  arr <- get_mon_arrivals(env)

  expect_equal(arr$start_time, c(1, 3, 5, 7, 9))
})
