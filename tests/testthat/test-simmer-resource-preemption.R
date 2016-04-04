context("resource-preemption")

test_that("a lower priority arrival gets rejected before accessing the server", {
  t0 <- create_trajectory() %>%
    seize("dummy", 1) %>%
    timeout(10) %>%
    release("dummy", 1)
  
  t1 <- create_trajectory() %>%
    seize("dummy", 1, priority=1, preemptible=1) %>%
    timeout(10) %>%
    release("dummy", 1)
  
  env <- simmer() %>%
    add_generator("p0a", t0, at(0, 0)) %>%
    add_generator("p1a", t1, at(2, 3)) %>%
    add_resource("dummy", 1, 2, preemptive=TRUE) %>%
    run()
  
  arrs <- env %>% get_mon_arrivals()
  arrs_ordered <- arrs[order(arrs$name),]
  
  expect_equal(as.character(arrs[!arrs$finished,]$name), "p0a1")
  expect_equal(arrs_ordered$end_time, c(30, 3, 12, 22))
})

test_that("tasks are NOT restarted", {
  t0 <- create_trajectory() %>%
    seize("dummy", 1, restart=FALSE) %>%
    timeout(10) %>%
    release("dummy", 1)
  
  t1 <- create_trajectory() %>%
    seize("dummy", 2, priority=1, preemptible=1) %>%
    timeout(10) %>%
    release("dummy", 2)
  
  env <- simmer() %>%
    add_generator("p0a", t0, at(0, 0)) %>%
    add_generator("p1a", t1, at(2, 15)) %>%
    add_resource("dummy", 2, preemptive=TRUE) %>%
    run()
  
  arrs <- env %>% get_mon_arrivals()
  arrs_ordered <- arrs[order(arrs$name),]
  
  expect_equal(arrs_ordered$end_time, c(30, 30, 12, 25))
  expect_equal(arrs_ordered$activity_time, c(10, 10, 10, 10))
})


test_that("tasks are restarted", {
  t0 <- create_trajectory() %>%
    seize("dummy", 1, restart=TRUE) %>%
    timeout(10) %>%
    release("dummy", 1)
  
  t1 <- create_trajectory() %>%
    seize("dummy", 2, priority=1, preemptible=1) %>%
    timeout(10) %>%
    release("dummy", 2)
  
  env <- simmer() %>%
    add_generator("p0a", t0, at(0, 0)) %>%
    add_generator("p1a", t1, at(2, 15)) %>%
    add_resource("dummy", 2, preemptive=TRUE) %>%
    run()
  
  arrs <- env %>% get_mon_arrivals()
  arrs_ordered <- arrs[order(arrs$name),]
  
  expect_equal(arrs_ordered$end_time, c(35, 35, 12, 25))
  expect_equal(arrs_ordered$activity_time, c(15, 15, 10, 10))
})

test_that("tasks are preempted in a FIFO basis", {
  t0 <- create_trajectory() %>%
    seize("dummy", 1, restart=TRUE) %>%
    timeout(10) %>%
    release("dummy", 1)
  
  t1 <- create_trajectory() %>%
    seize("dummy", 1, priority=1, preemptible=1) %>%
    timeout(10) %>%
    release("dummy", 1)
  
  env <- simmer() %>%
    add_generator("p0a", t0, at(0, 1)) %>%
    add_generator("p1a", t1, at(2, 3)) %>%
    add_resource("dummy", 2, preemptive=TRUE, preempt_order="fifo") %>%
    run()
  
  arrs <- env %>% get_mon_arrivals()
  arrs_ordered <- arrs[order(arrs$name),]
  
  expect_equal(arrs_ordered$end_time, c(22, 23 ,12, 13))
  expect_equal(arrs_ordered$activity_time, c(12, 12, 10, 10))
})

test_that("tasks are preempted in a LIFO basis", {
  t0 <- create_trajectory() %>%
    seize("dummy", 1, restart=TRUE) %>%
    timeout(10) %>%
    release("dummy", 1)
  
  t1 <- create_trajectory() %>%
    seize("dummy", 1, priority=1, preemptible=1) %>%
    timeout(10) %>%
    release("dummy", 1)
  
  env <- simmer() %>%
    add_generator("p0a", t0, at(0, 1)) %>%
    add_generator("p1a", t1, at(2, 3)) %>%
    add_resource("dummy", 2, preemptive=TRUE, preempt_order="lifo") %>%
    run()
  
  arrs <- env %>% get_mon_arrivals()
  arrs_ordered <- arrs[order(arrs$name),]
  
  expect_equal(arrs_ordered$end_time, c(22, 23, 12, 13))
  expect_equal(arrs_ordered$activity_time, c(13, 11, 10, 10))
})
