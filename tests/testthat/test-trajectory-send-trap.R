context("send/trap/untrap/wait")

test_that("an arrival waits", {
  t <- create_trajectory() %>%
    wait()

  env <- simmer(verbose = TRUE) %>%
    add_generator("dummy", t, at(0)) %>%
    run()

  expect_equal(env %>% peek, Inf)
  expect_equal(nrow(get_mon_arrivals(env)), 0)
})

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

test_that("a signal is untrapped", {
  t <- create_trajectory() %>%
    untrap("asdf") %>%
    send("signal", 3) %>%
    trap("signal") %>%
    timeout(1) %>%
    untrap("signal") %>%
    timeout(8) %>%
    timeout(1)

  env <- simmer(verbose = TRUE) %>%
    add_generator("dummy", t, at(0, 1, 2)) %>%
    run()
  arr <- get_mon_arrivals(env)

  expect_equal(arr$start_time, c(0, 1, 2))
  expect_equal(arr$end_time, c(10, 11, 12))
  expect_equal(arr$activity_time, c(10, 10, 10))
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

test_that("a signal is received while blocked inside a resource", {
  t <- create_trajectory() %>%
    send("signal", 3) %>%
    trap("signal") %>%
    seize("res", 1) %>%
    wait() %>%
    timeout(1) %>%
    release("res", 1)

  env <- simmer(verbose = TRUE) %>%
    add_resource("res", 1) %>%
    add_generator("dummy", t, at(0, 1, 2)) %>%
    run()
  arr_glb <- get_mon_arrivals(env)
  arr_res <- get_mon_arrivals(env, per_resource = TRUE)

  expect_equal(arr_glb$start_time, c(0, 1, 2))
  expect_equal(arr_glb$end_time, c(4, 5, 6))
  expect_equal(arr_glb$activity_time, c(1, 1, 1))
  expect_equal(arr_res$start_time, c(0, 1, 2))
  expect_equal(arr_res$end_time, c(4, 5, 6))
  expect_equal(arr_res$activity_time, c(1, 1, 1))
})

test_that("a signal is received while in a timeout inside a resource", {
  t <- create_trajectory() %>%
    send("signal", 3) %>%
    trap("signal") %>%
    seize("res", 1) %>%
    timeout(10) %>%
    timeout(1) %>%
    release("res", 1)

  env <- simmer(verbose = TRUE) %>%
    add_resource("res", 1) %>%
    add_generator("dummy", t, at(0, 1, 2)) %>%
    run()
  arr_glb <- get_mon_arrivals(env)
  arr_res <- get_mon_arrivals(env, per_resource = TRUE)

  expect_equal(arr_glb$start_time, c(0, 1, 2))
  expect_equal(arr_glb$end_time, c(4, 5, 6))
  expect_equal(arr_glb$activity_time, c(4, 1, 1))
  expect_equal(arr_res$start_time, c(0, 1, 2))
  expect_equal(arr_res$end_time, c(4, 5, 6))
  expect_equal(arr_res$activity_time, c(4, 1, 1))
})

test_that("a signal is ignored inside a batch (1)", {
  t <- create_trajectory() %>%
    send("signal", 3) %>%
    trap("signal") %>%
    batch(1) %>%
    seize("res", 1) %>%
    wait() %>%
    timeout(1) %>%
    release("res", 1)

  env <- simmer(verbose = TRUE) %>%
    add_resource("res", 1) %>%
    add_generator("dummy", t, at(0, 1, 2)) %>%
    run()
  arr_glb <- get_mon_arrivals(env)
  arr_res <- get_mon_arrivals(env, per_resource = TRUE)

  expect_equal(nrow(arr_glb), 0)
  expect_equal(nrow(arr_res), 0)
})

test_that("a signal is ignored inside a batch (2)", {
  t <- create_trajectory() %>%
    send("signal", 3) %>%
    trap("signal") %>%
    batch(1) %>%
    seize("res", 1) %>%
    timeout(10) %>%
    timeout(1) %>%
    release("res", 1)

  env <- simmer(verbose = TRUE) %>%
    add_resource("res", 1) %>%
    add_generator("dummy", t, at(0, 1, 2)) %>%
    run()
  arr_glb <- get_mon_arrivals(env)
  arr_res <- get_mon_arrivals(env, per_resource = TRUE)

  expect_equal(arr_glb$start_time, c(0, 1, 2))
  expect_equal(arr_glb$end_time, c(11, 22, 33))
  expect_equal(arr_glb$activity_time, c(11, 11, 11))
  expect_equal(arr_res$start_time, c(0, 1, 2))
  expect_equal(arr_res$end_time, c(11, 22, 33))
  expect_equal(arr_res$activity_time, c(11, 11, 11))
})
