context("timeout")

test_that("a non-function is correctly converted to a function by the value_to_func functions", {
  expect_error(not(create_trajectory()%>%timeout(3)))
})

test_that("a task function that returns a non-numeric value fails", {
  t0 <- create_trajectory() %>%
    timeout(function() {})
  
  env <- simmer() %>%
    add_generator("entity", t0, function() 1)
  
  expect_error(env%>%run(100))
})

test_that("a timeout is correctly monitored", {
  t0 <- create_trajectory() %>%
    timeout(-3)
  
  t1 <- create_trajectory() %>%
    timeout(3) %>%
    timeout(function() 4)
  
  env0 <- simmer() %>%
    add_generator("entity", t0, at(0)) %>%
    run()
  
  env1 <- simmer() %>%
    add_generator("entity", t1, at(0)) %>%
    run()
  
  expect_equal(get_mon_arrivals(env0)[1,]$end_time, 3)
  expect_equal(get_mon_arrivals(env1)[1,]$end_time, 7)
})
