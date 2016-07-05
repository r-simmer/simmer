context("seize/release")

test_that("resources are seized/released as expected", {
  t0 <- create_trajectory() %>%
    seize("dummy", -1) %>%
    timeout(1) %>%
    seize("dummy", function() 2) %>%
    timeout(1) %>%
    release("dummy", -1) %>%
    timeout(1) %>%
    release("dummy", function() 2) %>%
    timeout(1) %>%
    seize("dummy", 1)
  
  env <- simmer(verbose=TRUE) %>%
    add_resource("dummy", 3, 0) %>%
    add_generator("arrival", t0, at(0))
  
  env %>% onestep() %>% onestep()
  expect_equal(env %>% get_server_count("dummy"), 1)
  env %>% onestep() %>% onestep()
  expect_equal(env %>% get_server_count("dummy"), 3)
  env %>% onestep() %>% onestep() %>% onestep()
  expect_equal(env %>% get_server_count("dummy"), 2)
  env %>% onestep() %>% onestep()
  expect_equal(env %>% get_server_count("dummy"), 0)
})

test_that("incorrect types fail", {
  expect_error(create_trajectory() %>% seize(0, 0))
  expect_error(create_trajectory() %>% release(0, 0))
  expect_error(create_trajectory() %>% seize("dummy", "dummy"))
  expect_error(create_trajectory() %>% release("dummy", "dummy"))
})

test_that("arrivals perform a post.seize and then stop", {
  t <- create_trajectory() %>% 
    seize("dummy", 1, continue=FALSE, 
          post.seize = create_trajectory() %>% 
            timeout(2) %>%
            release("dummy", 1)) %>%
    timeout(1)
  
  env <- simmer(verbose=TRUE) %>%
    add_resource("dummy", 1, 0) %>%
    add_generator("arrival", t, at(0)) %>%
    run()
  arrs <- env %>% get_mon_arrivals()
  
  expect_true(arrs$finished)
  expect_equal(arrs$activity_time, 2)
})

test_that("arrivals can retry a seize", {
  t <- create_trajectory() %>% 
    seize("dummy", 1, continue=FALSE, 
          reject = create_trajectory() %>% 
            timeout(1) %>%
            rollback(2, Inf)) %>%
    timeout(2) %>%
    release("dummy", 1)
  
  env <- simmer(verbose=TRUE) %>%
    add_resource("dummy", 1, 0) %>%
    add_generator("arrival", t, at(0, 1)) %>%
    run()
  arrs <- env %>% get_mon_arrivals()
  
  expect_equal(arrs$start_time, c(0, 1))
  expect_equal(arrs$finished, c(TRUE, TRUE))
  expect_equal(arrs$activity_time, c(2, 3))
})

test_that("arrivals go through post.seize or reject and then continue", {
  t <- create_trajectory() %>% 
    seize("dummy", 1, continue=c(TRUE, TRUE), 
          post.seize = create_trajectory() %>% 
            timeout(2) %>%
            release("dummy"),
          reject = create_trajectory() %>% 
            timeout(3)) %>%
    timeout(3)
  
  env <- simmer(verbose=TRUE) %>%
    add_resource("dummy", 1, 0) %>%
    add_generator("arrival", t, at(0, 1)) %>%
    run()
  arrs <- env %>% get_mon_arrivals()
  
  expect_equal(arrs$start_time, c(0, 1))
  expect_equal(arrs$finished, c(TRUE, TRUE))
  expect_equal(arrs$activity_time, c(5, 6))
})
