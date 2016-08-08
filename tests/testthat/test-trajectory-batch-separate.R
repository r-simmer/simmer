context("batch/separate")

counter <- function() {
  n <- -1
  function() {
    n <<- n + 1
    n
  }
}

test_that("arrivals are batched", {
  t <- create_trajectory(verbose=TRUE) %>%
    batch(2, timeout=0, permanent=FALSE, rule=NULL) %>%
    seize("dummy", 1) %>%
    timeout(1) %>%
    release("dummy", 1) %>%
    #separate() %>%
    timeout(counter())
  
  env <- simmer(verbose=TRUE) %>%
    add_resource("dummy", 1, 0) %>%
    add_generator("arrival", t, at(0, 1, 2, 3)) %>%
    run()
  
  arr_glb <- get_mon_arrivals(env, per_resource=FALSE)
  arr_res <- get_mon_arrivals(env, per_resource=TRUE)
  
  expect_equal(arr_glb$start_time, c(0, 1, 2, 3))
  expect_equal(arr_glb$end_time, c(2, 2, 5, 5))
  expect_equal(arr_glb$activity_time, c(1, 1, 2, 2))
  expect_equal(arr_res$start_time, c(1, 1, 3, 3))
  expect_equal(arr_res$end_time, c(2, 2, 4, 4))
  expect_equal(arr_res$activity_time, c(1, 1, 1, 1))
})

test_that("batches are separated", {
  t <- create_trajectory(verbose=TRUE) %>%
    batch(2, timeout=0, permanent=FALSE, rule=NULL) %>%
    seize("dummy", 1) %>%
    timeout(1) %>%
    release("dummy", 1) %>%
    separate() %>%
    timeout(counter())
  
  env <- simmer(verbose=TRUE) %>%
    add_resource("dummy", 1, 0) %>%
    add_generator("arrival", t, at(0, 1, 2, 3)) %>%
    run()
  
  arr_glb <- get_mon_arrivals(env, per_resource=FALSE)
  arr_res <- get_mon_arrivals(env, per_resource=TRUE)
  
  expect_equal(arr_glb$start_time, c(0, 1, 2, 3))
  expect_equal(arr_glb$end_time, c(2, 3, 6, 7))
  expect_equal(arr_glb$activity_time, c(1, 2, 3, 4))
  expect_equal(arr_res$start_time, c(1, 1, 3, 3))
  expect_equal(arr_res$end_time, c(2, 2, 4, 4))
  expect_equal(arr_res$activity_time, c(1, 1, 1, 1))
})

test_that("permanent batches are NOT separated", {
  t <- create_trajectory(verbose=TRUE) %>%
    batch(2, timeout=0, permanent=TRUE, rule=NULL) %>%
    seize("dummy", 1) %>%
    timeout(1) %>%
    release("dummy", 1) %>%
    separate() %>%
    timeout(counter())
  
  env <- simmer(verbose=TRUE) %>%
    add_resource("dummy", 1, 0) %>%
    add_generator("arrival", t, at(0, 1, 2, 3)) %>%
    run()
  
  arr_glb <- get_mon_arrivals(env, per_resource=FALSE)
  arr_res <- get_mon_arrivals(env, per_resource=TRUE)
  
  expect_equal(arr_glb$start_time, c(0, 1, 2, 3))
  expect_equal(arr_glb$end_time, c(2, 2, 5, 5))
  expect_equal(arr_glb$activity_time, c(1, 1, 2, 2))
  expect_equal(arr_res$start_time, c(1, 1, 3, 3))
  expect_equal(arr_res$end_time, c(2, 2, 4, 4))
  expect_equal(arr_res$activity_time, c(1, 1, 1, 1))
})

test_that("a rule can prevent batching", {
  t <- create_trajectory(verbose=TRUE) %>%
    batch(2, timeout=0, permanent=FALSE, rule=function() 0) %>%
    seize("dummy", 1) %>%
    timeout(1) %>%
    release("dummy", 1) %>%
    separate() %>%
    timeout(counter())
  
  env <- simmer(verbose=TRUE) %>%
    add_resource("dummy", 1, 0) %>%
    add_generator("arrival", t, at(0, 1, 2, 3)) %>%
    run()
  
  arr_glb <- get_mon_arrivals(env, per_resource=FALSE)
  arr_res <- get_mon_arrivals(env, per_resource=TRUE)
  
  expect_equal(arr_glb$start_time, c(0, 1, 2, 3))
  expect_equal(arr_glb$end_time, c(1, 3, 5, 7))
  expect_equal(arr_glb$activity_time, c(1, 2, 3, 4))
  expect_equal(arr_res$start_time, c(0, 1, 2, 3))
  expect_equal(arr_res$end_time, c(1, 2, 3, 4))
  expect_equal(arr_res$activity_time, c(1, 1, 1, 1))
})

test_that("a timeout can trigger early batches", {
  t <- create_trajectory(verbose=TRUE) %>%
    batch(2, timeout=0.5, permanent=FALSE, rule=NULL) %>%
    seize("dummy", 1) %>%
    timeout(1) %>%
    release("dummy", 1) %>%
    separate() %>%
    timeout(counter())
  
  env <- simmer(verbose=TRUE) %>%
    add_resource("dummy", 1) %>%
    add_generator("arrival", t, at(0, 1, 2, 3)) %>%
    run()
  
  arr_glb <- get_mon_arrivals(env, per_resource=FALSE)
  arr_res <- get_mon_arrivals(env, per_resource=TRUE)
  
  expect_equal(arr_glb$start_time, c(0, 1, 2, 3))
  expect_equal(arr_glb$end_time, c(1.5, 3.5, 5.5, 7.5))
  expect_equal(arr_glb$activity_time, c(1, 2, 3, 4))
  expect_equal(arr_res$start_time, c(0.5, 1, 2.5, 3))
  expect_equal(arr_res$end_time, c(1.5, 2.5, 3.5, 4.5))
  expect_equal(arr_res$activity_time, c(1, 1, 1, 1))
})

test_that("all arrivals inside a batch store an attribute change", {
  t <- create_trajectory(verbose=TRUE) %>%
    batch(2, timeout=0, permanent=FALSE, rule=NULL) %>%
    set_attribute("asdf", 3) %>%
    separate()
  
  env <- simmer(verbose=TRUE) %>%
    add_resource("dummy", 1, 0) %>%
    add_generator("arrival", t, at(0, 1, 2, 3), mon=2) %>%
    run()
  
  attr <- get_mon_attributes(env)
  
  expect_equal(attr$time, c(1, 1, 3, 3))
  expect_equal(attr$key, c("asdf", "asdf", "asdf", "asdf"))
  expect_equal(attr$value, c(3, 3, 3, 3))
})

test_that("a shared name in different trajectories collects arrivals in the same batch", {
  t <- create_trajectory(verbose=TRUE) %>%
    seize("dummy", 1) %>%
    timeout(1) %>%
    release("dummy", 1) %>%
    #separate() %>%
    timeout(counter())
  
  t1 <- create_trajectory(verbose=TRUE) %>%
    batch(2, name = "asdf") %>%
    join(t)
  
  t2 <- create_trajectory(verbose=TRUE) %>%
    batch(2, name = "asdf") %>%
    join(t)
  
  env <- simmer(verbose=TRUE) %>%
    add_resource("dummy", 1, 0) %>%
    add_generator("arrival0", t1, at(0, 2)) %>%
    add_generator("arrival1", t2, at(1, 3)) %>%
    run()
  
  arr_glb <- get_mon_arrivals(env, per_resource=FALSE)
  arr_res <- get_mon_arrivals(env, per_resource=TRUE)
  
  expect_equal(arr_glb$start_time, c(0, 1, 2, 3))
  expect_equal(arr_glb$end_time, c(2, 2, 5, 5))
  expect_equal(arr_glb$activity_time, c(1, 1, 2, 2))
  expect_equal(arr_res$start_time, c(1, 1, 3, 3))
  expect_equal(arr_res$end_time, c(2, 2, 4, 4))
  expect_equal(arr_res$activity_time, c(1, 1, 1, 1))
})

test_that("unnamed batches in different trajectories collects arrivals in different batches", {
  t <- create_trajectory(verbose=TRUE) %>%
    seize("dummy", 1) %>%
    timeout(1) %>%
    release("dummy", 1) %>%
    #separate() %>%
    timeout(counter())
  
  t1 <- create_trajectory(verbose=TRUE) %>%
    batch(2, name = "") %>%
    join(t)
  
  t2 <- create_trajectory(verbose=TRUE) %>%
    batch(2, name = "") %>%
    join(t)
  
  env <- simmer(verbose=TRUE) %>%
    add_resource("dummy", 1, 0) %>%
    add_generator("arrival0", t1, at(0, 2)) %>%
    add_generator("arrival1", t2, at(1, 3)) %>%
    run()
  
  arr_glb <- get_mon_arrivals(env, per_resource=FALSE)
  arr_res <- get_mon_arrivals(env, per_resource=TRUE)
  
  expect_equal(arr_glb$start_time, c(0, 2, 1, 3))
  expect_equal(arr_glb$end_time, c(3, 3, 5, 5))
  expect_equal(arr_glb$activity_time, c(1, 1, 2, 2))
  expect_equal(arr_res$start_time, c(2, 2, 3, 3))
  expect_equal(arr_res$end_time, c(3, 3, 4, 4))
  expect_equal(arr_res$activity_time, c(1, 1, 1, 1))
})

test_that("nested batches' stats are correctly reported", {
  t <- create_trajectory(verbose=TRUE) %>%
    batch(2, timeout=0, permanent=FALSE, rule=NULL) %>%
    batch(2, timeout=0, permanent=FALSE, rule=NULL) %>%
    seize("dummy", 1) %>%
    timeout(1) %>%
    release("dummy", 1) %>%
    #separate() %>%
    #timeout(1) %>%
    #separate() %>%
    timeout(counter())
  
  env <- simmer(verbose=TRUE) %>%
    add_resource("dummy", 1, 0) %>%
    add_generator("arrival", t, at(0, 1, 2, 3)) %>%
    run()
  
  arr_glb <- get_mon_arrivals(env, per_resource=FALSE)
  arr_res <- get_mon_arrivals(env, per_resource=TRUE)
  
  expect_equal(arr_glb$start_time, c(0, 1, 2, 3))
  expect_equal(arr_glb$end_time, c(4, 4, 4, 4))
  expect_equal(arr_glb$activity_time, c(1, 1, 1, 1))
  expect_equal(arr_res$start_time, c(3, 3, 3, 3))
  expect_equal(arr_res$end_time, c(4, 4, 4, 4))
  expect_equal(arr_res$activity_time, c(1, 1, 1, 1))
})

test_that("nested batches are separated", {
  t <- create_trajectory(verbose=TRUE) %>%
    batch(2, timeout=0, permanent=FALSE, rule=NULL) %>%
    batch(2, timeout=0, permanent=FALSE, rule=NULL) %>%
    seize("dummy", 1) %>%
    timeout(1) %>%
    release("dummy", 1) %>%
    separate() %>%
    timeout(1) %>%
    separate() %>%
    timeout(counter())
  
  env <- simmer(verbose=TRUE) %>%
    add_resource("dummy", 1, 0) %>%
    add_generator("arrival", t, at(0, 1, 2, 3)) %>%
    run()
  
  arr_glb <- get_mon_arrivals(env, per_resource=FALSE)
  arr_res <- get_mon_arrivals(env, per_resource=TRUE)
  
  expect_equal(arr_glb$start_time, c(0, 1, 2, 3))
  expect_equal(arr_glb$end_time, c(5, 6, 7, 8))
  expect_equal(arr_glb$activity_time, c(2, 3, 4, 5))
  expect_equal(arr_res$start_time, c(3, 3, 3, 3))
  expect_equal(arr_res$end_time, c(4, 4, 4, 4))
  expect_equal(arr_res$activity_time, c(1, 1, 1, 1))
})
