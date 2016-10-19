context("send/trap/untrap/wait")

test_that("a signal is immediately triggered", {
  t <- create_trajectory() %>%
    send("signal")

  env <- simmer(verbose = TRUE) %>%
    add_generator("dummy", t, at(0))

  expect_output(env %>% run, ".*0.*Broadcast")
})

test_that("a signal is triggered with some delay", {
  t <- create_trajectory() %>%
    send("signal", 3)

  env <- simmer(verbose = TRUE) %>%
    add_generator("dummy", t, at(0))

  expect_output(env %>% run, ".*3.*Broadcast")
})

test_that("a signal is received while blocked", {
  t <- create_trajectory() %>%
    send("signal", 3) %>%
    trap("signal") %>%
    wait() %>%
    timeout(1)

  env <- simmer(verbose = TRUE) %>%
    add_generator("dummy", t, at(0, 1, 2)) %>%
    run()
  arr <- get_mon_arrivals(env)

  expect_equal(arr$start_time, c(0, 1, 2))
  expect_equal(arr$end_time, c(4, 4, 4))
  expect_equal(arr$activity_time, c(1, 1, 1))
})

test_that("a signal is received while in a timeout", {
  t <- create_trajectory() %>%
    send("signal", 3) %>%
    trap("signal") %>%
    timeout(10) %>%
    timeout(1)

  env <- simmer(verbose = TRUE) %>%
    add_generator("dummy", t, at(0, 1, 2)) %>%
    run()
  arr <- get_mon_arrivals(env)

  expect_equal(arr$start_time, c(0, 1, 2))
  expect_equal(arr$end_time, c(4, 4, 4))
  expect_equal(arr$activity_time, c(4, 3, 2))
})
