context("activity")

test_that("the activity chain grows as expected", {
  t0 <- create_trajectory() %>%
    seize("nurse", 1) %>%
    timeout(function() rnorm(1, 15)) %>%
    branch(function() 1, T, create_trajectory()%>%timeout(function() 1)) %>%
    release("nurse", 1)
  
  act <- get_next_activity(
    get_next_activity(
    get_next_activity(t0%>%get_head())))
  
  expect_output(show_activity(act), "Release")
  expect_output(show_activity(t0%>%get_tail()), "Release")
  expect_equal(get_next_activity(act), NULL)
})

test_that("a negative amount is converted to positive", {
  t0 <- create_trajectory()
  
  expect_silent(t0%>%seize("nurse", -2))
  expect_silent(t0%>%release("nurse", -10))
})

test_that("a non-function duration fails", {
  expect_error(create_trajectory()%>%timeout(3))
})

test_that("a duration function that returns a non-numeric value fails", {
  t0 <- create_trajectory() %>%
    timeout(function() {})
  
  env <- simmer() %>%
    add_generator("entity", t0, function() 1)
  
  expect_error(env%>%run(100))
})

test_that("we can force some errors (just to complete coverage)", {
  expect_error(create_trajectory() %>% get_next_activity())
  
  t0 <- create_trajectory() %>% timeout(function() {})
  t0$.__enclos_env__$private$head <- NULL
  expect_error(t0 %>% show_trajectory())
  expect_error(t0$.__enclos_env__$private$add_activity(NULL))
  
  t0 <- create_trajectory() %>% timeout(function() {})
  t0$.__enclos_env__$private$tail <- NULL
  expect_error(t0 %>% timeout(function() {}))
})
