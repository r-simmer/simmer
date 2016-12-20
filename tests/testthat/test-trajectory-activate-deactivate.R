context("activate/deactivate")

test_that("several deactivates don't crash", {
  t <- trajectory() %>%
    deactivate("dummy")

  env <- simmer(verbose = TRUE) %>%
    add_generator("dummy", t, at(0, 0, 1)) %>%
    run()

  expect_equal(now(env), 1)
})

test_that("generators are deactivated and activated again as expected", {
  t <- trajectory() %>%
    deactivate("dummy") %>%
    timeout(1) %>%
    activate("dummy")

  env <- simmer(verbose = TRUE) %>%
    add_generator("dummy", t, function() 1) %>%
    run(10)
  arr <- get_mon_arrivals(env)

  expect_equal(arr$start_time, c(1, 3, 5, 7))
  expect_equal(arr$end_time, c(2, 4, 6, 8))
})
