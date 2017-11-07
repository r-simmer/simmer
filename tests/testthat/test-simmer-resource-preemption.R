context("resource-preemption")

test_that("a lower priority arrival gets rejected before accessing the server", {
  t <- trajectory() %>%
    seize("dummy", 1) %>%
    timeout(10) %>%
    release("dummy", 1)

  env <- simmer(verbose = TRUE) %>%
    add_generator("p0a", t, at(0, 0)) %>%
    add_generator("p1a", t, at(2, 3), priority = 1) %>%
    add_resource("dummy", 1, 2, preemptive = TRUE) %>%
    run()

  arrs <- env %>% get_mon_arrivals()
  arrs_ordered <- arrs[order(arrs$name), ]

  expect_equal(as.character(arrs[!arrs$finished, ]$name), "p0a1")
  expect_equal(arrs_ordered$end_time, c(30, 3, 12, 22))
})

test_that("tasks are NOT restarted", {
  t0 <- trajectory() %>%
    seize("dummy", 1) %>%
    timeout(10) %>%
    release("dummy", 1)
  t1 <- trajectory() %>%
    seize("dummy", 2) %>%
    timeout(10) %>%
    release("dummy", 2)

  env <- simmer(verbose = TRUE) %>%
    add_generator("p0a", t0, at(0, 0), restart = FALSE) %>%
    add_generator("p1a", t1, at(2, 15), priority = 1) %>%
    add_resource("dummy", 2, preemptive = TRUE) %>%
    run()

  arrs <- env %>% get_mon_arrivals()
  arrs_ordered <- arrs[order(arrs$name), ]

  expect_equal(arrs_ordered$end_time, c(30, 30, 12, 25))
  expect_equal(arrs_ordered$activity_time, c(10, 10, 10, 10))
})


test_that("tasks are restarted", {
  t0 <- trajectory() %>%
    seize("dummy", 1) %>%
    timeout(10) %>%
    release("dummy", 1)
  t1 <- trajectory() %>%
    seize("dummy", 2) %>%
    timeout(10) %>%
    release("dummy", 2)

  env <- simmer(verbose = TRUE) %>%
    add_generator("p0a", t0, at(0, 0), restart = TRUE) %>%
    add_generator("p1a", t1, at(2, 15), priority = 1) %>%
    add_resource("dummy", 2, preemptive = TRUE) %>%
    run()

  arrs <- env %>% get_mon_arrivals()
  arrs_ordered <- arrs[order(arrs$name), ]

  expect_equal(arrs_ordered$end_time, c(35, 35, 12, 25))
  expect_equal(arrs_ordered$activity_time, c(15, 15, 10, 10))
})

test_that("tasks are preempted in a FIFO basis", {
  t <- trajectory() %>%
    seize("dummy", 1) %>%
    timeout(10) %>%
    release("dummy", 1)

  env <- simmer(verbose = TRUE) %>%
    add_generator("p0a", t, at(0, 1), restart = TRUE) %>%
    add_generator("p1a", t, at(2, 3), priority = 1) %>%
    add_resource("dummy", 2, preemptive = TRUE, preempt_order = "fifo") %>%
    run()

  arrs <- env %>% get_mon_arrivals()
  arrs_ordered <- arrs[order(arrs$name), ]

  expect_equal(arrs_ordered$end_time, c(22, 23, 12, 13))
  expect_equal(arrs_ordered$activity_time, c(12, 12, 10, 10))
})

test_that("tasks are preempted in a LIFO basis", {
  t <- trajectory() %>%
    seize("dummy", 1) %>%
    timeout(10) %>%
    release("dummy", 1)

  env <- simmer(verbose = TRUE) %>%
    add_generator("p0a", t, at(0, 1), restart = TRUE) %>%
    add_generator("p1a", t, at(2, 3), priority = 1) %>%
    add_resource("dummy", 2, preemptive = TRUE, preempt_order = "lifo") %>%
    run()

  arrs <- env %>% get_mon_arrivals()
  arrs_ordered <- arrs[order(arrs$name), ]

  expect_equal(arrs_ordered$end_time, c(22, 23, 12, 13))
  expect_equal(arrs_ordered$activity_time, c(13, 11, 10, 10))
})

test_that("queue can exceed queue_size by default", {
  t <- trajectory() %>%
    seize("dummy", 1) %>%
    timeout(10) %>%
    release("dummy", 1)

  env <- simmer(verbose = TRUE) %>%
    add_generator("p0a", t, at(0, 0)) %>%
    add_generator("p1a", t, at(1), priority = 1) %>%
    add_resource("dummy", 1, 1, preemptive = TRUE) %>%
    run()

  res <- env %>% get_mon_resources()
  arr <- env %>% get_mon_arrivals()
  arr_ordered <- arr[order(arr$name), ]

  expect_equal(res$time, c(0, 0, 1, 11, 20, 30))
  expect_equal(res$server, c(1, 1, 1, 1, 1, 0))
  expect_equal(res$queue, c(0, 1, 2, 1, 0, 0))
  expect_equal(arr_ordered$end_time, c(20, 30, 11))
  expect_equal(arr_ordered$activity_time, c(10, 10, 10))
  expect_equal(arr_ordered$finished, c(TRUE, TRUE, TRUE))
})

test_that("queue cannot exceed queue_size with hard limit (preempted rejected)", {
  t <- trajectory() %>%
    seize("dummy", 1) %>%
    timeout(10) %>%
    release("dummy", 1)

  env <- simmer(verbose = TRUE) %>%
    add_generator("p0a", t, at(0, 0)) %>%
    add_generator("p1a", t, at(1), priority = 1) %>%
    add_resource("dummy", 1, 1, preemptive = TRUE, queue_size_strict = TRUE) %>%
    run()

  res <- env %>% get_mon_resources()
  arr <- env %>% get_mon_arrivals()
  arr_ordered <- arr[order(arr$name), ]

  expect_equal(res$time, c(0, 0, 1, 11, 21))
  expect_equal(res$server, c(1, 1, 1, 1, 0))
  expect_equal(res$queue, c(0, 1, 1, 0, 0))
  expect_equal(arr_ordered$end_time, c(1, 21, 11))
  expect_equal(arr_ordered$activity_time, c(1, 10, 10))
  expect_equal(arr_ordered$finished, c(FALSE, TRUE, TRUE))
})

test_that("queue cannot exceed queue_size with hard limit (preempted to queue)", {
  t <- trajectory() %>%
    seize("dummy", 1) %>%
    timeout(10) %>%
    release("dummy")

  env <- simmer(verbose = TRUE) %>%
    add_generator("p0a", t, at(0), priority = 0) %>%
    add_generator("p1a", t, at(0), priority = 1) %>%
    add_generator("p2a", t, at(1), priority = 2) %>%
    add_resource("dummy", 1, 1, preemptive = TRUE, queue_size_strict = TRUE) %>%
    run()

  res <- env %>% get_mon_resources()
  arr <- env %>% get_mon_arrivals()
  arr_ordered <- arr[order(arr$name), ]

  expect_equal(res$time, c(0, 0, 1, 11, 20))
  expect_equal(res$server, c(1, 1, 1, 1, 0))
  expect_equal(res$queue, c(0, 1, 1, 0, 0))
  expect_equal(arr_ordered$end_time, c(1, 20, 11))
  expect_equal(arr_ordered$activity_time, c(0, 10, 10))
  expect_equal(arr_ordered$finished, c(FALSE, TRUE, TRUE))
})

test_that("preemption works in non-saturated multi-server resources", {
  low_prio <- trajectory() %>%
    seize("res", 1) %>%
    timeout(10) %>%
    release("res", 1)

  high_prio <- trajectory() %>%
    seize("res", 7) %>%
    timeout(10) %>%
    release("res", 7)

  env <- simmer(verbose = TRUE) %>%
    add_resource("res", 10, preemptive = TRUE) %>%
    add_generator("low_prio", low_prio, at(rep(0, 5))) %>%
    add_generator("high_prio", high_prio, at(1), priority = 1) %>%
    run()

  arr <- get_mon_arrivals(env)

  expect_equal(arr$start_time, c(0, 0, 0, 1, 0, 0))
  expect_equal(arr$end_time, c(10, 10, 10, 11, 19, 19))
  expect_equal(arr$activity_time, rep(10, 6))
})
