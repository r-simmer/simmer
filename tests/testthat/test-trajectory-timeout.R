context("timeout")

test_that("a task function that returns a non-numeric value fails", {
  t0 <- create_trajectory() %>%
    timeout(function() {})
  
  env <- simmer() %>%
    add_generator("entity", t0, function() 1)
  
  expect_error(env%>%run(100))
})

test_that("a timeout is correctly monitored", {
  t0 <- create_trajectory() %>%
    seize("dummy") %>%
    timeout(-3) %>%
    release("dummy")
  
  t1 <- create_trajectory() %>%
    seize("dummy") %>%
    timeout(3) %>%
    timeout(function() 4) %>%
    release("dummy")
  
  env0 <- simmer() %>%
    add_generator("entity", t0, at(0)) %>%
    add_resource("dummy") %>%
    run()
  
  env1 <- simmer() %>%
    add_generator("entity", t1, at(0)) %>%
    add_resource("dummy") %>%
    run()
  
  expect_equal(get_mon_arrivals(env0)[1,]$end_time, 3)
  expect_equal(get_mon_arrivals(env0, TRUE)[1,]$end_time, 3)
  expect_equal(get_mon_arrivals(env1)[1,]$end_time, 7)
  expect_equal(get_mon_arrivals(env1, TRUE)[1,]$end_time, 7)
})

test_that("incorrect types fail", {
  expect_error(create_trajectory() %>% timeout("dummy"))
})
