context("activity")

test_that("the activity chain grows as expected", {
  t0 <- Trajectory$new() $
    seize("nurse", 1) $
    timeout(function() rnorm(1, 15)) $
    release("nurse", 1)
  act <- t0$get_head()$next_activity$next_activity
  
  expect_equal(act, t0$get_tail())
  expect_equal(act$next_activity, NULL)
})

test_that("a negative amount is converted to positive", {
  t0 <- Trajectory$new() $
    seize("nurse", -2) $
    release("nurse", -10)
  
  expect_equal(t0$get_head()$.__enclos_env__$private$amount, 2)
  expect_equal(t0$get_tail()$.__enclos_env__$private$amount, 10)
})

test_that("a non-function duration fails", {
  expect_error(Trajectory$new()$timeout(3))
})

test_that("a duration function that returns a non-numeric value fails", {
  t0 <- Trajectory$new() $
    timeout(function() {})
  
  simmer <- Simmer$new() $
    add_generator("entity", t0, function() 1)
  
  expect_error(simmer$run(100))
})
