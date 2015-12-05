context("rollback")

test_that("a rollback points to the correct activity", {
  t0 <- create_trajectory() %>%
    seize("nurse", 1) %>%
    timeout(function() rnorm(1, 15)) %>%
    branch(function() 1, T, create_trajectory()%>%timeout(function() 1)) %>%
    rollback(3)
  expect_output(t0%>%get_tail()%>%print_activity(), "Seize")
  
  t0 <- create_trajectory() %>%
    seize("nurse", 1) %>%
    timeout(function() rnorm(1, 15)) %>%
    branch(function() 1, T, create_trajectory()%>%timeout(function() 1)) %>%
    rollback(30)
  expect_output(t0%>%get_tail()%>%print_activity(), "Seize")
})

test_that("a rollback loops the correct number of times", {
  t0 <- create_trajectory() %>% rollback(0, 3)
  
  env <- simmer(verbose=TRUE) %>%
    add_generator("dummy", t0, function() 1)
  
  expect_output(env%>%run(2), 
"sim: anonymous | time: 1 | arrival: dummy0 | activity: Rollback(none)
sim: anonymous | time: 1 | arrival: dummy0 | activity: Rollback(none)
sim: anonymous | time: 1 | arrival: dummy0 | activity: Rollback(none)
sim: anonymous | time: 1 | arrival: dummy0 | activity: Rollback(none)", fixed=T)
  
  t0 <- create_trajectory() %>% rollback(0, Inf)
  expect_output(t0%>%get_tail()%>%print_activity(), "Inf")
})

test_that("a negative amount is converted to positive", {
  t0 <- create_trajectory() %>% seize("dummy", 1)

  expect_silent(t0%>%rollback(-1, -1))
  expect_output(t0%>%get_tail()%>%print_activity(), "amount: 1 (Seize), times: 1", fixed=TRUE)
})

test_that("a check function that returns a non-boolean value fails", {
  t0 <- create_trajectory() %>%
    rollback(1, check=function() "dummy")
  
  env <- simmer() %>%
    add_generator("entity", t0, function() 1)
  
  expect_error(env%>%run(100))
})

test_that("incorrect types fail", {
  expect_error(create_trajectory() %>% rollback("dummy", 0))
  expect_error(create_trajectory() %>% rollback(0, "dummy"))
  expect_error(create_trajectory() %>% rollback(0, check=0))
})
